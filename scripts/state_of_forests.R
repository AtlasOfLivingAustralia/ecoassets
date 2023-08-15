# creates state/territory and national datasets sent to Cressida at ABARES for
# state of forests report

ds <- open_dataset("data/galah", format = "parquet")

dir.create("data/tmp_sof")

ds |> 
  filter(year >= 1970,
         cl22 != "",
         kingdom == "Plantae" | phylum == "Chordata",
         cl10000 != "",
         scientificName != "",
         taxonRank %in% c("species", "variety", "subspecies", 
                          "cultivar", "subvariety", "form"),
         str_detect(speciesID, "https://")) |>
  mutate(forestType = case_when(
    cl10000 == 'Non forest' ~ "nonForest",
    TRUE ~ "forest")) |> 
  select(speciesID,
         kingdom,
         phylum,
         class,
         order,
         family,
         genus,
         vernacularName,
         scientificName,
         stateTerritory = cl22,
         forestType,
         taxonRank) |>
  write_dataset(path = "data/tmp_sof", format = "parquet")

# ISSUE_01:
# some records are duplicated without vernacularNames, and sometimes 
# conflicting vernacularNames exist for subspecies
# here, collapse subspecies etc. into species, and use the vernacularName and 
# scientificName corresponding to taxonRank  == species

ds_plantae_chordata <- open_dataset("data/tmp_sof")

list_of_names <- ds_plantae_chordata |> 
  distinct(speciesID, vernacularName, scientificName, taxonRank) |>
  group_by(speciesID) |> 
  collect() |> 
  mutate(count_speciesID = n()) 

# confirms species is always present after group_by(speciesID)
janitor::tabyl(list_of_names, count_speciesID, taxonRank)

common_names <- list_of_names |> 
  group_by(speciesID) |> 
  filter(taxonRank == "species") |> 
  select(speciesID, scientificName, vernacularName)

grouped_counts <- ds_plantae_chordata |> 
  group_by(speciesID,
           kingdom,
           phylum,
           class,
           order,
           family,
           genus,
           stateTerritory,
           forestType) |> 
  summarise(counts = n()) |>
  ungroup() |>
  left_join(common_names, by = join_by(speciesID)) |> 
  collect()
              
# ISSUE_02:
# duplicated records once grouped by taxonomy i.e. different speciesID but
# everything else is the same
# group_by everything except speciesID, sum counts, and join to previous to
# get just one (random) speciesID

collapsed_counts <- grouped_counts |> 
  group_by(kingdom, 
           phylum, 
           class, 
           order, 
           family,
           genus, 
           scientificName, 
           vernacularName, 
           stateTerritory,
           forestType) |> 
  summarise(all_counts = sum(counts)) |> 
  ungroup() |> 
  left_join(grouped_counts, by = join_by(kingdom, 
                                         phylum,
                                         class,
                                         order,
                                         family,
                                         genus, 
                                         scientificName,
                                         vernacularName, 
                                         stateTerritory,
                                         forestType)) |> 
  select(-counts) |> 
  group_by(kingdom, 
           phylum, 
           class, 
           order, 
           family,
           genus, 
           scientificName, 
           vernacularName, 
           stateTerritory,
           forestType, 
           all_counts) |> 
  slice_head()

# state counts
state_counts <- collapsed_counts |>
  pivot_wider(names_from = forestType,
              values_from = all_counts,
              values_fill = 0) |>
  rowwise() |>
  mutate(allRecords = sum(forest, nonForest)) |>
  rowwise() |>
  mutate(percentInForest = (round(forest / allRecords * 100))) |> 
  relocate(speciesID, scientificName) 

saveRDS(state_counts, "data/state_of_forests/state_counts.rds")
write_csv_arrow(state_counts, "data/state_of_forests/state_counts.csv")  

# national counts
national_counts <- state_counts |> 
  select(-c(stateTerritory, percentInForest)) |> 
  group_by(speciesID,
           scientificName,
           kingdom, 
           phylum, 
           class, 
           order,
           family, 
           genus, 
           vernacularName) |> 
  summarise(forest = sum(forest),
            nonForest = sum(nonForest), 
            allRecords = sum(allRecords)) |> 
  rowwise() |> 
  mutate(percentInForest = (round(forest / allRecords * 100)))

saveRDS(national_counts, "data/state_of_forests/national_counts.rds")
write_csv_arrow(national_counts, "data/state_of_forests/national_counts.csv")  

unlink("data/tmp_sof", recursive = TRUE)
