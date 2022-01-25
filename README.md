# About
In this project we costumize a Docker image to create a graph model in [Neo4j](https://neo4j.com) using the [MovieLens dataset](https://grouplens.org/datasets/movielens/).

## Download and prepare the data
Follow the instructions at [data/README.md](data/README.md) before proceeding with the next step.

## Build Docker image
Build the costumized image with:
```bash
docker build -t jruizvar/movielens-neo4j .
```

## Run Docker image
To run our costumized image with the Graph Data Science library as a plugin, execute:

```bash
./run_movilens_neo4j.sh
```