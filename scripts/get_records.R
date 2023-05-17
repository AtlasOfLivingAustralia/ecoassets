# downloads records necessary for EcoAssets summary datasets and runs 
# checks against the downloaded data 

years <- as.numeric(c(1900:2022))

get_occ <- function(each_year) {

  y <- tibble(name =
                c("id",
                  "lft",
                  "rgt",
                  "speciesID",
                  "taxonConceptID",
                  "kingdom",
                  "phylum",
                  "class",
                  "order",
                  "family",
                  "genus",
                  "species",
                  "scientificName",
                  "vernacularName",
                  "infraspecificEpithet",
                  "taxonRank",
                  "decimalLatitude",
                  "decimalLongitude",
                  "basisOfRecord",
                  "year",
                  "eventID",
                  "dataResourceUid",
                  "dataResourceName",
                  "samplingProtocol",
                  "cl111032",
                  "cl11032",
                  "cl111033",
                  "cl11033",
                  "cl10902",
                  "cl10000",
                  "cl22",
                  "cl1048",
                  "cl966"),
              type = "field")

  attr(y, "call") <- "galah_select"

  x <- galah_call() |>
    galah_apply_profile(ALA) |>
    galah_filter(year == my_year,
                 decimalLatitude != "",
                 decimalLongitude != "",
                 speciesID != "")

  x$select <- y

  x |>
    atlas_occurrences() |>
    filter(!is.na(cl966) | !is.na(cl1048)) |>
    write_parquet(sink = paste0("data/galah/occ_", each_year, ".parquet"))

  # Sys.sleep(60)

}

walk(years, get_occ)

# quick check of downloaded data
ds <- open_dataset("data/galah", format = "parquet")
cat(length(list.files("data/galah/")), 
    "files have been downloaded. These data contain", 
    ncol(ds), 
    "columns and span the following years:",
    min(collect((select(ds, year)))), 
    "-",
    max(collect((select(ds, year)))))

# TODO: add {pointblank} tests to check data download
# ds <- open_dataset("data/galah", format = "parquet")
# con <- dbConnect(duckdb::duckdb())
# duckdb_register_arrow(con, "ds", ds)
# duck_ds <- tbl(con, "ds")
# agent <-
#   create_agent(tbl = duck_ds) |> 
#   col_count_match(count = 33) |> 
#   interrogate()
