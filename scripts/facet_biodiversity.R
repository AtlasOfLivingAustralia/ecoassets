# brief description

# aggregated biodiversity dataset --------

dir.create("data/tmp_ds")
dir.create("data/tmp_agg")

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
    mutate(year = year_id, .before = 1) |> 
    write_parquet(sink = paste0("data/tmp_agg/year_", year_id, ".parquet"))

}

walk(pq_files, sum_occurrences)

open_dataset("data/tmp_agg", format = "parquet") |> 
  write_csv_arrow(here("data",
                       "aggregated_aus_species_occ",
                       "aggregated_aus_species_occ.csv"))


# summary datasets --------

agg_ds <- open_dataset("data/tmp_agg", 
                       format = "parquet",
                       schema = schema(basisOfRecord = string(),
                                       stateTerritory = string(), 
                                       ibraRegion = string(),
                                       imcraRegion = string(),
                                       forest2018Status = string(),
                                       forest2013Status = string(),
                                       capadStatus = string(),
                                       epbcStatus = string(), 
                                       griisStatus = string(),
                                       speciesID = large_utf8(),
                                       speciesName = string(),
                                       occurrenceCount = int32(),
                                       year = int32()))  

lookup_year_grp <- tibble(year = 1900:2020,
                          grp = c(rep(LETTERS[1], times = 71),
                                  rep(LETTERS[2:11], each = 5)))

lookup_start_end_grp <- tibble(yearStart = c(1900, seq(1971, 2016, 5)),
                               yearEnd = c(1970, seq(1975, 2020, 5)),
                               grp = LETTERS[1:11])


### terrestrial introduced ------

agg_ds |> 
  select(ibraRegion, 
         year, 
         griisStatus, 
         speciesID, 
         occurrenceCount) |> 
  filter(!is.na(ibraRegion),
         year <= 2020) |> 
  left_join(lookup_year_grp, by = join_by(year)) |>
  group_by(ibraRegion, grp, griisStatus) |>
  summarise(recordCount = sum(occurrenceCount),
            speciesCount = n_distinct(speciesID),
            .groups = "drop") |>
  left_join(lookup_start_end_grp, by = join_by(grp)) |> 
  select(ibraRegion, 
         yearStart, 
         yearEnd, 
         griisStatus,
         recordCount, 
         speciesCount) |> 
  write_csv_arrow(here("data",
                       "summary_introduced_spp_occ_terrestrial",
                       "summary_introduced_spp_occ_terrestrial.csv"))


### marine introduced ------

agg_ds |> 
  select(imcraRegion, 
         year, 
         griisStatus, 
         speciesID, 
         occurrenceCount) |> 
  filter(!is.na(imcraRegion),
         year <= 2020) |> 
  left_join(lookup_year_grp, by = join_by(year)) |>
  group_by(imcraRegion, grp, griisStatus) |>
  summarise(recordCount = sum(occurrenceCount),
            speciesCount = n_distinct(speciesID),
            .groups = "drop") |>
  left_join(lookup_start_end_grp, by = join_by(grp)) |> 
  select(imcraRegion, 
         yearStart, 
         yearEnd, 
         griisStatus,
         recordCount, 
         speciesCount) |> 
  write_csv_arrow(here("data",
                       "summary_introduced_spp_occ_marine",
                       "summary_introduced_spp_occ_marine.csv"))


### terrestrial threatened ---------

agg_ds |> 
  select(ibraRegion, 
         year, 
         epbcStatus, 
         speciesID, 
         occurrenceCount) |> 
  filter(!is.na(ibraRegion),
         year <= 2020) |> 
  left_join(lookup_year_grp, by = join_by(year)) |>
  group_by(ibraRegion, grp, epbcStatus) |>
  summarise(recordCount = sum(occurrenceCount),
            speciesCount = n_distinct(speciesID),
            .groups = "drop") |>
  left_join(lookup_start_end_grp, by = join_by(grp)) |> 
  select(ibraRegion, 
         yearStart, 
         yearEnd, 
         epbcStatus,
         recordCount, 
         speciesCount) |> 
  write_csv_arrow(here("data",
                       "summary_threatened_spp_occ_terrestrial",
                       "summary_threatened_spp_occ_terrestrial.csv"))


### marine threatened -------------

agg_ds |> 
  select(imcraRegion, 
         year, 
         epbcStatus, 
         speciesID, 
         occurrenceCount) |> 
  filter(!is.na(imcraRegion),
         year <= 2020) |> 
  left_join(lookup_year_grp, by = join_by(year)) |>
  group_by(imcraRegion, grp, epbcStatus) |>
  summarise(recordCount = sum(occurrenceCount),
            speciesCount = n_distinct(speciesID),
            .groups = "drop") |>
  left_join(lookup_start_end_grp, by = join_by(grp)) |> 
  select(imcraRegion, 
         yearStart, 
         yearEnd, 
         epbcStatus,
         recordCount, 
         speciesCount) |> 
  write_csv_arrow(here("data",
                       "summary_threatened_spp_occ_marine",
                       "summary_threatened_spp_occ_marine.csv"))


### protection status terrestrial -------

### protection status marine --------

unlink("data/tmp_ds", recursive = TRUE)
unlink("data/tmp_agg", recursive = TRUE)

