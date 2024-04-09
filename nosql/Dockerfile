FROM neo4j:4.4.26-community

WORKDIR /var/lib/neo4j

COPY data/movies.csv data/users.csv data/ratings.csv ./import/

RUN mkdir scripts
COPY create_genre_nodes.cypher ./scripts
COPY split_train_test.cypher ./scripts
COPY compute_biases.cypher ./scripts
COPY init-neo4j.sh ./scripts

# # Set the entrypoint to the init script
# ENTRYPOINT ["scripts/init-neo4j.sh"]

# Import data and create genre nodes and relationships
RUN neo4j-admin import \
    --database=movielens \
    --nodes=Movie=import/movies.csv \
    --nodes=User=import/users.csv \
    --relationships=RATE=import/ratings.csv
RUN sed -i '9s/#dbms.default_database=neo4j/dbms.default_database=movielens/' conf/neo4j.conf