# downloads records necessary for EcoAssets summary datasets and runs 
# checks against the downloaded data 

# 01. get data ------
# NOTE: OR filters (|) not working in {galah}, so manually filter IBRA/IMCRA 
# in downstream wrangling to restrict records to Australia only 
# i.e. (cl1048 != "" | cl966 != "")

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
    write_parquet(sink = paste0("data/galah/occ_", my_year))
  
  Sys.sleep(600)
  
}

years <- as.numeric(c(1900:2022))
map(years, get_occ)

# 02. check downloaded data ------
ds <- open_dataset("data/galah", format = "parquet")
duck_tbl <- ds |> to_duckdb()

# i. correct number of columns
# ii. years are within the expected range
# iii. speciesID, lat, and lon fields don't contain blanks
agent <- create_agent(tbl = duck_tbl,
                      tbl_name = "get_records",
                      label = "Check validity of get_records.R") |>
  col_count_match(count = 33) |> 
  col_vals_between(columns = vars(year), left = min(years), right = max(years)) |>
  col_vals_not_null(columns = vars(decimalLatitude, decimalLongitude, speciesID)) |> 
  interrogate()

x_write_disk(agent, filename = "agent-get_records", path = "tests")  

