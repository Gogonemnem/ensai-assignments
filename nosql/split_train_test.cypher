MATCH (u:User)-[r:RATE]->(m:Movie)
WITH r, u, rand() AS randomValue
SET r.set = CASE WHEN randomValue < 0.8 THEN 'train' ELSE 'test' END

