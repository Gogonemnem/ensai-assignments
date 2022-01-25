# About
In this project we costumize a Docker image to create a graph model in [Neo4j](https://neo4j.com) using the [MovieLens dataset](https://grouplens.org/datasets/movielens/).

## Download and prepare the data
Follow the instructions at [data](data) before proceeding with the next step.

## Build and run Docker image
```bash
docker compose up -d
```

## [Neo4j Browser](http://localhost:7474)
Navigate to [localhost:7474](http://localhost:7474) to run queries upon our movielens graph data model.