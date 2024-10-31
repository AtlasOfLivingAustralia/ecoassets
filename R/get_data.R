#' Get occurrences
#'
#' `get_occ()` downloads and saves occurrence records from the ALA as parquet
#' files
#'
#' @param each_year The year to filter records to
#'
#' @return One parquet file saved to the "data/galah/" directory, where the
#'   filename is prefixed with "occ_" and ends with the year provided in the
#'   `each_year` param
#' @export
get_occ <- function(each_year) {
  
  res <- galah_call() |>
    galah_apply_profile(ALA) |>
    select(recordID, 
           lft, 
           rgt, 
           speciesID, 
           taxonConceptID,
           kingdom, 
           phylum, 
           class, 
           order, 
           family,
           genus,
           species,
           scientificName,
           vernacularName,
           infraspecificEpithet,
           taxonRank,
           decimalLatitude,
           decimalLongitude,
           basisOfRecord,
           year,
           eventID,
           dataResourceUid,
           dataResourceName,
           samplingProtocol,
           cl111032,
           cl11032,
           cl111033,
           cl11033,
           cl10902,
           cl10000,
           cl22,
           cl1048,
           cl966) |>
    filter(year == each_year,
           decimalLatitude != "",
           decimalLongitude != "",
           speciesID != "",
           (!is.na(cl966) | !is.na(cl1048))) |>
    atlas_occurrences() 
  
  res |> 
    write_parquet(sink = paste0("data/galah/occ_", each_year, ".parquet"))
  
}

#' Get left and right values from the API for each taxonomic rank being queried
#'
#' @param query The URL string to use as a query on the namematching API
#'
#' @return A tibble with five columns: the name of the taxonomic rank being
#'   queried, the left and right values associated with that taxonomic rank,
#'   details of the match type, and whether there were any issues associated
#'   with the query
#' @export
get_lft_rgt <- function(query) {
  x <- GET(query)
  y <- fromJSON(rawToChar(x$content))
  tax_rank <- y$rank
  tibble(!!tax_rank := y[[tax_rank]],
         left = y$lft,
         right = y$rgt,
         match_type = y$matchType,
         issues = y$issues)
  }