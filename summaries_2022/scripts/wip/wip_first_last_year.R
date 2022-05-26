
# derive earliest and latest dates per species per region

library(here)
library(dplyr)
library(vroom)

# get the data -----
occ <- vroom(here("summaries_2022",
                  "data",
                  "processed",
                  "grouped_occ.csv"),
             col_select = c(speciesId, year, locationID))

loc <- vroom(here("summaries_2022",
                  "data",
                  "processed",
                  "distinct_loc.csv"),
             col_select = c(ibraRegion, imcraRegion, locationID))

tax <- vroom(here("summaries_2022",
                  "data",
                  "processed",
                  "distinct_taxon_griis.csv"),
             col_select = c(speciesId, speciesName))

joined <- occ |> 
  full_join(loc, by = "locationID") |> 
  full_join(tax, by = "speciesId") |> 
  select(-c(speciesId, locationID)) |> 
  filter(!is.na(year))


# get dates of first and last record ------
# IBRA
ibra <- joined |> 
  select(-imcraRegion) |> 
  filter(!is.na(ibraRegion)) |> 
  group_by(speciesName, ibraRegion) |> 
  summarise(earliestRecord = min(year), 
            latestRecord = max(year))

# IMCRA 
imcra <- joined |> 
  select(-ibraRegion) |> 
  filter(!is.na(imcraRegion)) |> 
  group_by(speciesName, imcraRegion) |> 
  summarise(earliestRecord = min(year), 
            latestRecord = max(year))




capad <- vroom(here("summaries_2022",
                  "data",
                  "processed",
                  "grouped_occ.csv"),






















