FROM neo4j:4.4.3-community

WORKDIR /var/lib/neo4j

COPY data/movies.csv data/users.csv data/ratings.csv ./import

RUN neo4j-admin import \
    --database=movielens \
    --nodes=Movie=import/movies.csv \
    --nodes=User=import/users.csv \
    --relationships=RATE=import/ratings.csv && \
    sed -i '9s/#dbms.default_database=neo4j/dbms.default_database=movielens/' conf/neo4j.conf