
library(RSQLite)
library(DBI)
library(dplyr)
library(here)
library(duckdb)
library(vroom)

# upload functions -----
# sqlite
csv_2_sqlite <- function(sqlite_file, csv_file, table_name) {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
  readr::read_delim_chunked(file =  csv_file,
                            callback = function(x, pos) {
                              dbWriteTable(con, x, name = table_name, append = TRUE)
                            },
                            delim = ",")
  
  # check tables in sqlite db 
  all_tables <- dbListTables(con)
  print(all_tables)
  
  # have a look at the first 10 rows
  rows_10 <- dplyr::tbl(con, table_name) |> 
    head(n = 10L) |> 
    dplyr::collect()
  print(rows_10)
  
  DBI::dbDisconnect(con)
  beepr::beep(8)
}


# duckdb using function
csv_2_duckdb <- function(duck_file, csv_file, table_name) {
  
  con <- DBI::dbConnect(duckdb::duckdb(), duck_file)
  readr::read_delim_chunked(file =  csv_file,
                            callback = function(x, pos) {
                              dbWriteTable(con, x, name = table_name, append = TRUE)
                            },
                            delim = ",",
                            chunk_size = 1000000)
  
  # check tables in duckdb 
  all_tables <- dbListTables(con)
  print(all_tables)
  
  # have a look at the first 10 rows
  rows_10 <- dplyr::tbl(con, table_name) |> 
    head(n = 10L) |> 
    dplyr::collect()
  print(rows_10)
  
  DBI::dbDisconnect(con)
  beepr::beep(8)
}

# duckdb directly

t1 <- Sys.time()
con <- DBI::dbConnect(duckdb::duckdb(), "ducky.duckdb")
dbWriteTable(con, "test_direct", test_all)
DBI::dbDisconnect(con)
t2 <- Sys.time()
t2-t1

# test summary performance ------
# sqlite

t1 <- Sys.time()
con <- dbConnect(RSQLite::SQLite(), "mysqlite.sqlite")

# intermediate table with all unique coords, year, basis of record, species
sqlite_res <- tbl(con, "data") |> 
  filter(country == "Australia",
         taxonRank == "species" | taxonRank == "subspecies",
         year > 1900) |> 
  group_by(scientificName, year, basisOfRecord) |> # coords need to be added
  summarise(count = n()) |> 
  collect()

dbDisconnect(con)
t2 <- Sys.time()
t2-t1


# duckdb

t3 <- Sys.time()
con <- dbConnect(duckdb::duckdb(), "ducky.duckdb")

# intermediate table with all unique coords, year, basis of record, species
duck_res <- tbl(con, "extract") |> 
  filter(country == "Australia",
         taxonRank == "species" | taxonRank == "subspecies",
         year > 1900) |> 
  group_by(scientificName, year, basisOfRecord) |> # coords need to be added
  summarise(count = n()) |> 
  collect()

dbDisconnect(con)
t4 <- Sys.time()
t4-t3






subset_data <- data.table::fread("extract4gb.csv", nrows = 30000)

t1 <- Sys.time()  
test_all <- vroom("extract4gb.csv", delim = ",")
t2 <- Sys.time()
t2-t1


# options -------
# 1. data.table::fread()
# 2. RSQLite
# 3. sqldf
# 4. inborutils
# 5. {arrow} + {duckdb}
# 6. vroom


# option 1 ------

library(data.table)
dat <- fread("extract4gb.csv")
Error: vector memory exhausted (limit reached?)


# option 2 ------

library(RSQLite)
con <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
dbWriteTable(con, "extract16feb", "extract4gb.csv") 

# option 3 -----



# option 4 -------

library(inborutils)
library(readr)

inborutils::csv_to_sqlite(csv_file = "extract4gb.csv", 
                          "testdb.sqlite", 
                          "ala_extract", 
                          pre_process_size = 1000, 
                          chunk_size = 50000, 
                          show_progress_bar = TRUE)


# option 5 -------

library(arrow)
library(duckdb)
library(DBI)

con <- dbConnect(duckdb::duckdb(), "db.duckdb")
dbWriteTable(con, "extract4gb", "extract4gb.csv")
dbGetQuery(con, 'SELECT "Species", MIN("Sepal.Width") FROM iris GROUP BY "Species"')


# option 6 -----

library(data.table)
library(vroom)

rownames_ref <- data.table::fread("extract4gb.csv", nrows = 50000)
df <- vroom("extract4gb.csv", col_select = c(-eventDate, -'array_join(multiValues[speciesListUid] AS `speciesListUid`, |)'))




# script to test out different reading and querying options
# read_csv_arrow() - crashes
# duckdb_read_csv() - input errors, probs delim or seps
# vroom into R, then dbWriteTable - vroom is good, dbWriteTable takes forever
# vroom into R, then write_parquet() - write_parquet takes forever


library(arrow)
library(dplyr)
library(DBI)
library(duckdb)
library(vroom)
library(readr)

# read_csv_arrow() --------
# df <- read_csv_arrow("extract4gb.csv")


# duckdb_read_csv --------
# con <- dbConnect(duckdb::duckdb(), "my-db.duckdb")
# duckdb_read_csv(con, "extract", "extract4gb.csv")
# dbDisconnect(con)


# vroom to read into R, then dbWriteTable ------
# df <- vroom("extract4gb.csv")
# Time difference of 2.13957 mins
# con <- dbConnect(duckdb::duckdb(), "my-db.duckdb")
# dbWriteTable(con, "extract", df)
# dbDisconnect(con)


# vroom to read into R, then write_parquet() ------
# df <- vroom("extract4gb.csv")
# write_parquet(df, "extract.parquet")


# read in as arrow table - crashes R ------
# my_csv <- read_csv_arrow("data/ala-20220309.csv", as_data_frame = FALSE)


# query directly with {arrow} -----
ds <- open_dataset("data", format = "csv")
ds$schema
system.time(ds %>%
              select(
                id, 
                scientificName, 
                basisOfRecord, 
                decimalLatitude, 
                decimalLongitude, 
                country, 
                taxonRank, 
                year) |> 
              filter(country == "Australia",
                     taxonRank == "species" | taxonRank == "subspecies",
                     year > 1900) |> 
              group_by(scientificName, year, basisOfRecord) |> 
              collect() |> 
              summarise(count = n()) %>%
              print())


