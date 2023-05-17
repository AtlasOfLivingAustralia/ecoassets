# downloads, checks, and saves records for downstream aggregation 
# TODO: include a test that checks if all values of any column are NULL/NA
# this should safeguard against layers being disabled

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
    galah_filter(year == each_year,
                 decimalLatitude != "",
                 decimalLongitude != "",
                 speciesID != "")

  x$select <- y

  x |>
    atlas_occurrences() |>
    filter(!is.na(cl966) | !is.na(cl1048)) |>
    col_count_match(count = nrow(y)) |> 
    col_vals_not_null(columns = vars(decimalLatitude, 
                                     decimalLongitude, 
                                     speciesID, 
                                     year)) |> 
    write_parquet(sink = paste0("data/galah/occ_", each_year, ".parquet"))

}

walk(years, get_occ)
