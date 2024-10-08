#' Get data
#'
#' `get_data()` downloads and saves occurrence records from the ALA as parquet
#' files
#'
#' @param each_year The year to filter records to
#'
#' @return One parquet file saved to the "data/galah/" directory, where the
#'   filename is prefixed with "occ_" and ends with the year provided in the
#'   `each_year` param
#' @export
get_data <- function(each_year) {
  
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