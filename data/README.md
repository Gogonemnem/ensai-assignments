# MovieLens Latest Datasets

The small version of MovieLens dataset contains 100000 ratings and 3600 tag applications applied to 9000 movies by 600 users.
To proceed with the graph model we require three files:

- `movies.csv`
- `users.csv`
- `ratings.csv`

The command below downloads and prepares those files, when running on linux:
```bash
./download_and_prepare_data_linux.sh
```
For macOs, use instead: 
```bash
./download_and_prepare_data_macos.sh
```