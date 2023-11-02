CALL apoc.periodic.iterate(
  "MATCH (u:User)-[r:RATE]->(m:Movie) RETURN r",
  "WITH r, rand() AS randomValue SET r.set = CASE WHEN randomValue < 0.8 THEN 0 ELSE 1 END",
  {batchSize:10000, parallel:true}
) YIELD batches, total RETURN batches, total;

// Calculate global average rating
MATCH ()-[r:RATE {set: 0}]->()
WITH AVG(r.rating) AS mu

CALL apoc.periodic.iterate(
  "MATCH (u:User) RETURN u",
  "OPTIONAL MATCH (u)-[r:RATE {set: 0}]->() WITH u, AVG(r.rating) AS avgRating WHERE avgRating IS NOT NULL SET u.bias = avgRating - $mu",
  {batchSize:100, params:{mu: mu}, parallel:true}
)  YIELD batches, total RETURN batches, total;

// Calculate global average rating
MATCH ()-[r:RATE {set: 0}]->()
WITH AVG(r.rating) AS mu

CALL apoc.periodic.iterate(
  "MATCH (m:Movie) RETURN m",
  "OPTIONAL MATCH (m)<-[r:RATE {set: 0}]-() WITH m, AVG(r.rating) AS avgRating WHERE avgRating IS NOT NULL SET m.bias = avgRating - $mu",
  {batchSize:100, params:{mu: mu}, parallel:true}
)  YIELD batches, total RETURN batches, total;

// 1. Drop and create the projected graph
MATCH (i:Intermediate) DELETE i;

CALL gds.graph.exists('mySubgraph') YIELD exists
WHERE exists = true
// If it exists, drop the graph
CALL gds.graph.drop('mySubgraph') YIELD graphName
RETURN graphName + " dropped" AS result;

MATCH (source) OPTIONAL MATCH (source:User)-[r:RATE]->(target:Movie)
WHERE r.set = 0
WITH gds.graph.project('mySubgraph', source, target, { sourceNodeLabels: labels(source),
    targetNodeLabels: labels(target), relationshipProperties: r { .rating } }) AS g
RETURN
  g.graphName AS graph, g.nodeCount AS nodes, g.relationshipCount AS rels;

// 2. Compute and store filtered node similarity
CALL apoc.periodic.iterate(
  "CALL gds.alpha.nodeSimilarity.filtered.stream('mySubgraph', {sourceNodeFilter:'User', targetNodeFilter:'User', topK:10}) YIELD node1, node2, similarity RETURN node1, node2, similarity",
  "WITH gds.util.asNode(node1) AS sourceNode, gds.util.asNode(node2) AS targetNode, similarity CREATE (sourceNode)-[:SIMILAR {score: similarity}]->(targetNode)",
  {batchSize:10000, parallel:true}
)  YIELD batches, total RETURN batches, total;

// 3. Use the computed similarities in your recommendation system
CALL apoc.periodic.iterate(
    '// Calculate global average rating
    MATCH ()-[r:RATE {set: 0}]->()
    WITH AVG(r.rating) AS mu
    MATCH (u:User)-[rtest:RATE {set: 1}]->(targetMovie:Movie) 
    RETURN u, targetMovie, rtest.rating AS actualRating, mu',
     
    '
    MATCH (u:User)-[rtest:RATE {set: 1}]->(targetMovie:Movie) 
    MATCH (u)-[sim:SIMILAR]->(u2:User)
    WHERE u <> u2 AND EXISTS((u2)-[:RATE {set: 0}]->(targetMovie))

    WITH u, targetMovie, mu, u2, rtest.rating AS actualRating, sim

    MATCH (u2:User)-[r:RATE {set: 0}]->(targetMovie)
    WHERE u <> u2
    WITH u, mu, sim, u2, r, actualRating, targetMovie

    WITH u, mu, SUM(sim.score * (r.rating - u2.bias - targetMovie.bias - mu)) AS numerator, SUM(sim.score) AS denominator, actualRating, targetMovie
    WHERE denominator <> 0
    WITH mu + u.bias + targetMovie.bias + numerator / denominator AS predictedRating, actualRating


    CREATE (i:Intermediate {predictedRating: predictedRating, actualRating: actualRating})
    RETURN i',
    { batchSize: 16, parallel: false }
) YIELD batches, total

// 4. Compute RMSE using Intermediate nodes
MATCH (i:Intermediate)
WITH SUM((i.predictedRating - i.actualRating)^2) AS totalSquaredError, COUNT(i) AS count
RETURN sqrt(totalSquaredError/count) AS RMSE;