// Calculate global average rating
MATCH ()-[r:RATE {set: 'train'}]->()
WITH AVG(r.rating) AS mu

// Store user biases
MATCH (u:User)
OPTIONAL MATCH (u)-[r:RATE {set: 'train'}]->()
WITH u, mu, COALESCE(AVG(r.rating), 0) AS userAvgRating
SET u.bias = userAvgRating - mu

// Store movie biases
WITH mu
MATCH (m:Movie)
OPTIONAL MATCH (m)<-[r:RATE {set: 'train'}]-()
WITH m, mu, COALESCE(AVG(r.rating), 0) AS movieAvgRating
SET m.bias = movieAvgRating - mu
