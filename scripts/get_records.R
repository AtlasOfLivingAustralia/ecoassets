# Downloads and saves records as parquet files prior to downstream aggregation 

years <- as.numeric(c(1900:2023))

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

walk(years, get_occ)
