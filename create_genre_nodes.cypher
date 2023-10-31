// Create Genre nodes if they don't already exist
MATCH (m:Movie)
UNWIND split(m.genres, '|') AS genre
MERGE (g:Genre {name: genre});

// Match movies and genres and create relationships
MATCH (m:Movie)
UNWIND split(m.genres, '|') AS genre
MATCH (g:Genre {name: genre})
MERGE (m)-[:IN_GENRE]->(g);

