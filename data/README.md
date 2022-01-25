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
:START_ID(User),:END_ID(Movie),rating:float,timestamp:int
```