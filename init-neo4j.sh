#!/bin/bash

# Start Neo4j
neo4j start

# Wait for Neo4j to fully start (adjust the sleep time if needed)
sleep 30 

# Execute Cypher commands (replace with your Neo4j credentials)
cypher-shell -u neo4j -p test < scripts/create_genre_nodes.cypher
cypher-shell -u neo4j -p test < scripts/split_train_test.cypher
cypher-shell -u neo4j -p test < scripts/compute_biases.cypher

# Optionally, you can keep the service running in the foreground
# This is typically required for Docker containers to keep running
tail -f /var/lib/neo4j/logs/neo4j.log
