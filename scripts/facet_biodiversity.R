
# 6 faceted datasets and 1 large aggregated one


# relational tables -------
loc <- open_dataset(here("data", "interim", "rel_distinct_loc.parquet"),
                    format = "parquet",
                    schema = schema(locationID = int32(),
                                    decimalLatitude = double(),
                                    decimalLongitude = double(),
                                    stateTerritory = string(),
                                    ibraRegion = string(),
                                    imcraRegion = string(),
                                    forest2018Status = string(),
                                    forest2013Status = string(),
                                    capadStatus = string()))


occ <- open_dataset(here("data", "interim", "rel_occ_counts.parquet"),
                    format = "parquet",
                    schema = schema(speciesID = large_utf8(), 
                                    year = int32(),
                                    basisOfRecord = string(),
                                    counts = int32(),
                                    epbcStatus = string(),
                                    locationID = int32()))

taxa <- open_dataset(here("data", "interim", "rel_distinct_taxa.parquet"),
                    format = "parquet",
                    schema = schema(speciesID = large_utf8(),
                                    kingdom = string(),
                                    phylum = string(),
                                    class = string(),
                                    order = string(),
                                    family = string(),
                                    genus = string(),
                                    speciesName = string(),
                                    griisStatus = string()))


# aggregated data ------
dir.create("data/tmp_ds")
dir.create("data/tmp_agg")

occ |> 
  left_join(taxa, by = join_by(speciesID)) |> 
  select(-c(kingdom, phylum, class, order, family, genus)) |>
  left_join(loc, by = join_by(locationID)) |> 
  select(-c(locationID, decimalLatitude, decimalLongitude)) |> 
  group_by(year) |> 
  write_dataset(path = "data/tmp_ds", format = "parquet")




pq_files <- list.files("data/tmp_ds", full.names = TRUE, recursive = TRUE)

sum_occurrences <- function(fpath) {
  
  year_id <- sub('.*=(.*?)/.*', '\\1', fpath)
  
  fpath |> 
    read_parquet() |> 
    group_by(basisOfRecord, 
             stateTerritory,
             ibraRegion,
             imcraRegion,
             forest2018Status,
             forest2013Status,
             capadStatus, 
             epbcStatus,
             griisStatus, 
             speciesID, 
             speciesName) |> 
    summarise(occurrenceCount = sum(counts), .groups = "drop") |> 
    mutate(year = year_id) |> 
    write_parquet(sink = paste0("data/tmp_agg/year_", year_id, ".parquet"))

}

walk(pq_files, sum_occurrences)

open_dataset("data/tmp_agg", format = "parquet") |> 
  write_csv_arrow(here("data",
                       "processed",
                       "aggregated_data_aus_species_occ",
                       "aggregated_data_aus_species_occ.csv"))



# TODO: generate facets from agg data

agg_ds <- open_dataset("data/tmp_agg", format = "parquet")
agg_data <- vroom::vroom("AggregatedData.csv", n_max = 100)



unlink("data/tmp_ds", recursive = TRUE)
unlink("data/tmp_agg", recursive = TRUE)








