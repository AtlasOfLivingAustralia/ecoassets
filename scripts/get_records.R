# script to loop over records by year and download using {galah}
# TODO: add EPBC and GRIIS once indexed, pointblank checks

library(galah)
library(purrr)
library(arrow)
library(dplyr)

galah_config(email = Sys.getenv("email"), verbose = TRUE)

# downloads records for selected year, with a pause between API calls
# OR filters (currently) not working in {galah}, so manually filter IBRA/IMCRA 
# (cl1048 IS NOT NULL OR cl966 IS NOT NULL)
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

### tests ------
# 1 file for every year
#length(list.files("data/galah")) == length(years) 
# number of columns downloaded matches selected fields
#expect_col_count_match(tbl, count = tbl_2)
