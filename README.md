# Neo4j Docker Configuration

To run a Neo4j container with the Graph Data Science library as a plugin, you can run
```bash
docker run \
    --name movielens \
    -p7474:7474 -p7687:7687 \
    -d \
    -v $HOME/neo4j/data:/data \
    -v $HOME/neo4j/logs:/logs \
    -v $HOME/neo4j/import:/var/lib/neo4j/import \
    -v $HOME/neo4j/plugins:/plugins \
    --env NEO4J_AUTH=neo4j/test \
    --env NEO4JLABS_PLUGINS='["graph-data-science"]' \
    neo4j:latest
```

Once the instance starts, we can import the [MovieLens dataset](https://grouplens.org/datasets/movielens/) following the instructions at [data](data/README.md).
