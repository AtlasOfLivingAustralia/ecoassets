# downloads records necessary for EcoAssets summary datasets and runs 
# checks against the downloaded data 

years <- as.numeric(c(1900:2022))

get_occ <- function(my_year) {
  
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
    write_dataset(path = "data/galah", 
                  format = "parquet", 
                  basename_template = paste0("{i}", "_occ_", my_year, ".parquet"))
  
  Sys.sleep(600)
  
}

map(years, get_occ)

# check downloaded data has:
# correct number of columns
# years are within the expected range
# speciesID, lat, and lon fields don't contain blanks

ds <- open_dataset("data/galah", format = "parquet")

duck_tbl <- ds |> to_duckdb()

agent <- create_agent(tbl = duck_tbl,
                      tbl_name = "get_records",
                      label = "Check validity of get_records.R") |>
  col_count_match(count = 33) |> 
  col_vals_between(columns = vars(year), left = min(years), right = max(years)) |>
  col_vals_not_null(columns = vars(decimalLatitude, decimalLongitude, speciesID)) |> 
  interrogate()

x_write_disk(agent, filename = "agent-get_records", path = "tests")  

