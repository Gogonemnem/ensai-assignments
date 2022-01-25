# MovieLens Latest Datasets

The small version of MovieLens dataset contains 100000 ratings and 3600 tag applications applied to 9000 movies by 600 users.
We can retrieve this dataset with the following instructions:
```bash
wget https://files.grouplens.org/datasets/movielens/ml-latest-small.zip
unzip ml-latest-small.zip
```

To proceed with the graph model we require three files:

- `movies.csv`
- `users.csv`
- `ratings.csv`

### `movies.csv`

We can verify the number of lines in the file `movies.csv` with the command
```bash
wc -l movies.csv
```

We need to modify the header of the file in order to specify the `ID` column:
```
movieId:ID(Movie),title,genres
```

### `users.csv`

The MovieLens dataset does not provide a separate file with the list of users. However, we can obtain this file with the following instructions:
```bash
cat ratings.csv | cut -f1 -d , | uniq > users.csv
```

Once again, we need to modify the header of the file to:
```
userId:ID(User)
```

### `ratings.csv`

In order to create relationship between two nodes, the `IDs` defined in `movies.csv` and `users.csv` are used for the `:START_ID` and `:END_ID` fields. We establish this relationship in the header of the `ratings.csv` file:

```
:START_ID,:END_ID,rating:float,timestamp:int
```

# Importing the data

We need to move the files into the volume shared with the docker container:

```bash
mv movies.csv $HOME/neo4j/import/
mv users.csv $HOME/neo4j/import/
mv ratings.csv $HOME/neo4j/import/
```

To verify that the files are visible inside the container, we can inspect interactively:
```bash
docker exec -it movielens bash
```
Inside the container, we can verify the first lines of the `movies.csv` file
```bash
head import/movies.csv
``` 

### [`neo4j-admin import`](https://neo4j.com/docs/operations-manual/current/tools/neo4j-admin/neo4j-admin-import/)

The command `neo4j-admin import` allows to do batch imports of large amounts of data into a Neo4j database from CSV files. In our case, the complete instruction looks like this: 
```bash
bin/neo4j-admin import \
    --database=movielens \
    --nodes=Movie=import/movies.csv \
    --nodes=User=import/users.csv \
    --relationships=RATE=import/ratings.csv
```

## Changing default database
The community version of Neo4j has some nuances to work with multiple databases. We need to change the default database in the `conf/neo4j.conf` to our recently created database:

```bash
sed -i '9s/#dbms.default_database=neo4j/dbms.default_database=movielens/' conf/neo4j.conf
```

After restarting the container, we should be able to find our database:

```bash
bin/cypher-shell -u neo4j -p test 'SHOW DATABASES'
```