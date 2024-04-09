#!/bin/bash

echo 'Download and unzip dataset'
wget https://files.grouplens.org/datasets/movielens/ml-latest-small.zip
unzip ml-latest-small.zip ml-latest-small/movies.csv ml-latest-small/ratings.csv
mv ml-latest-small/movies.csv movies.csv
mv ml-latest-small/ratings.csv ratings.csv

echo 'Creating users.csv'
cat ratings.csv | cut -f1 -d , | uniq > users.csv

echo 'Fixing header of movies.csv'
sed -i '1s/movieId/movieId:ID(Movie)/' movies.csv

echo 'Fixing header of users.csv'
sed -i '1s/userId/userId:ID(User)/' users.csv

echo 'Fixing header of ratings.csv'
sed -i '1s/userId/:START_ID(User)/' ratings.csv
sed -i '1s/movieId/:END_ID(Movie)/' ratings.csv
sed -i '1s/rating/rating:float/' ratings.csv
sed -i '1s/timestamp/timestamp:int/' ratings.csv

echo 'Removing temp files'
rm -r ml-latest-small

echo 'Data preparation complete!'