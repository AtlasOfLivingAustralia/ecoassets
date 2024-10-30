#' Summarise events
#'
#' @param fpath A filepath to read
#' @export

summarise_events <- function(fpath) {
  
  year_id <- sub('.*=(.*?)/.*', '\\1', fpath)
  
  fpath |> 
    read_parquet() |> 
    group_by(decimalLatitude,
             decimalLongitude,
             cl22, 
             cl1048,
             cl966,
             eventID,
             dataResourceUid,
             dataResourceName, 
             samplingProtocol) |> 
    summarise(occCount = n(),
              speciesCount = n_distinct(species),
              minLeft = min(lft),
              maxRight = max(rgt),
              .groups = "drop") |> 
    mutate(year = year_id, .before = 1) |> 
    write_parquet(sink = paste0("data/tmp_events_agg/year_", year_id, ".parquet"))
  
}