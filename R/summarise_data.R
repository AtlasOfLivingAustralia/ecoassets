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

#' Summarise proportions
#'
#' @param df 
#' @param region_type 
#' @param status_val 
#'
#' @export

get_proportions <- function(df, region_type, status_val) {
  
  df |> 
    group_by({{region_type}}, yearStart, yearEnd, status) |>
    mutate(groupedSpeciesCount = sum(speciesCount)) |>
    ungroup() |> 
    distinct({{region_type}}, yearStart, yearEnd, status, groupedSpeciesCount) |> 
    group_by({{region_type}}, yearStart, yearEnd) |>
    mutate(totalCount = sum(groupedSpeciesCount)) |>
    ungroup() |> 
    filter(status == status_val) |>
    rowwise() |>
    mutate(prop = groupedSpeciesCount / totalCount) |>
    ungroup() |> 
    mutate(period = paste0(yearStart, "—", yearEnd))
}

