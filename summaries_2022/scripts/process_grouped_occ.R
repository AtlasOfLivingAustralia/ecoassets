
# get grouped counts of speciesId, year, locationID, basisOfRecord, epbcStatus
# substitute decimalLat and decimalLon for locationID from distinct_loc
# add epbc status to occurrence 

library(here)
library(dplyr)
library(arrow)
library(galah)
library(data.table)


occ <- open_dataset(here("summaries_2022", 
                         "data", 
                         "raw",
                         "aws", 
                         "grouped_occ_raw.csv"), 
                    format = "csv")

# add EPBC status to grouped_occ -------

# EPBC list from Cam via DAWE
epbc_list <- read_csv_arrow(here("summaries_2022", 
                                 "data", 
                                 "raw",
                                 "20220503spcs.csv"))

# match to names in ALA
epbc_ala_raw <- as.data.frame(search_taxa(epbc_list$`Scientific Name`))

# only retain complete matches
epbc_ala_tidy <- epbc_ala_raw %>% 
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) %>% 
  select(search_term, scientific_name, taxon_concept_id)

# epbc list with matched names
epbc <- inner_join(epbc_list,
                   epbc_ala_tidy,
                   by = c("Scientific Name" = "search_term")) |>
  select(scientific_name, `Threatened status`)
  

# join epbc to grouped_occ
add_epbc <- occ |> 
  left_join(epbc, by = c("speciesName" = "scientific_name")) |> 
  mutate(epbcStatus = case_when(
    is.na(`Threatened status`) ~ "Not listed",
    TRUE ~ as.character(`Threatened status`))) |> 
  select(-c(speciesName, `Threatened status`))


# add locationID to grouped_occ -------
loc_subset <- read_csv_arrow(here("summaries_2022", 
                           "data", 
                           "processed", 
                           "distinct_loc.csv")) |> 
  select(locationID, decimalLatitude, decimalLongitude)


# join locationId column 
interm_join <- add_epbc |> 
  left_join(loc_subset, by = c("decimalLatitude", "decimalLongitude")) |>   
  select(-c("decimalLatitude", "decimalLongitude")) |>   
  collect() 

# convert to datatable object to speed things up
# use magrittr pipes instead because base pipes don't get along with . as placeholder
grouped_occ <- interm_join %>%
  setDT() %>%
  filter(!duplicated(.))
  
fwrite(grouped_occ, here("summaries_2022", 
                                  "data", 
                                  "processed", 
                                  "grouped_occ.csv"))
