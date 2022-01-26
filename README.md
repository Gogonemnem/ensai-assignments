# About
In this project we costumize a Docker image to create a graph model in [Neo4j](https://neo4j.com) using the [MovieLens dataset](https://grouplens.org/datasets/movielens/).

## Download and prepare the data
Follow the instructions at [data](data) before proceeding with the next step.

## Build and run Docker image
```bash
docker compose up -d
```

## Using constrains 
You can also specify unique constraints that guarantee uniqueness of a certain property on nodes with a specific label.
```bash
docker exec movielens-neo4j-neo4j-1 cypher-shell -u neo4j -p test 'CREATE CONSTRAINT ON (n:User) ASSERT n.userId IS UNIQUE'
```
```bash
docker exec movielens-neo4j-neo4j-1 cypher-shell -u neo4j -p test 'CREATE CONSTRAINT ON (n:Movie) ASSERT n.movieId IS UNIQUE'
```

## [Neo4j Browser](http://localhost:7474)
Navigate to [localhost:7474](http://localhost:7474) to run queries upon the movielens graph data model. For instance, a list of movies ordered by popularity can be obtained with the following query:
```sql
MATCH p=()-->(n)
WITH n.title AS title, count(p) AS popularity
RETURN title, popularity
ORDER BY popularity DESC
LIMIT 10
```