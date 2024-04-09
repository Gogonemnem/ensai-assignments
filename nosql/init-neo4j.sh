#!/bin/bash

# Start Neo4j
neo4j start

# Wait for Neo4j to fully start
until (echo > /dev/tcp/localhost/7687) >/dev/null 2>&1; do
    sleep 1
done

# Restart Neo4j
neo4j stop
sleep 5
neo4j start

# Wait for Neo4j to fully start
until (echo > /dev/tcp/localhost/7687) >/dev/null 2>&1; do
    sleep 1
done


# # Execute Cypher commands (replace with your Neo4j credentials)
cypher-shell -u neo4j -p test < scripts/create_genre_nodes.cypher
cypher-shell -u neo4j -p test < scripts/split_train_test.cypher
cypher-shell -u neo4j -p test < scripts/compute_biases.cypher

# Optionally, you can keep the service running in the foreground
# This is typically required for Docker containers to keep running
tail -f /var/lib/neo4j/logs/neo4j.log
