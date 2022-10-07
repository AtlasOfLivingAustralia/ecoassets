
# process 3 AWS datasets: distinct_loc, distinct_taxon, grouped_occ

library(here)
library(arrow)
library(dplyr)
library(data.table)
library(galah)


# 1. distinct_loc -----
# replace "capad_terrestrial" and "capad_marine" with a single capadStatus value 
# which should be one of c("IPA", "PA", "Not protected") regardless of whether 
# that comes from the terrestrial or the marine layer
# add a unique numeric key to each row in distinct_loc ("locationID") and 
# use that in grouped_occ instead of lat/lon coordinates to make joins simpler

loc <- open_dataset(here("data", "raw", "distinct_loc_aws.csv"), format = "csv")

# get values for fields that need wrangling
forest_2018_vals <- loc |> 
  distinct(forest2018) |> 
  collect()

forest_2013_vals <- loc |> 
  distinct(forest2013) |> 
  collect()

capad_t_vals <- loc |> 
  distinct(capad_t_class) |> 
  collect()

capad_m_vals <- loc |> 
  distinct(capad_m_class) |> 
  collect()

# wrangle
distinct_loc <- loc |> 
  mutate(
    forest2018Status = case_when(
      forest2018 %in% c("Non forest", "") ~ "non-forest",
      TRUE ~ "forest"),
    forest2013Status = case_when(
      forest2013 %in% c("Non Forest", "") ~ "non-forest",
      TRUE ~ "forest"),
    capad_m_class = case_when(
      capad_m_class == "Indigenous Protected Area" ~ "IPA",
      capad_m_class == "" ~ "not protected",
      TRUE ~ "PA"), 
    capad_t_class = case_when(
      capad_t_class %in% c("Indigenous Protected Area", "Aboriginal Area") ~ "IPA",
      capad_t_class == "" ~ "not protected",
      TRUE ~ "PA"),
    capadStatus = case_when(
      capad_m_class == "IPA" | capad_t_class == "IPA" ~ "IPA",
      capad_m_class != "IPA" & capad_t_class != "IPA" & capad_m_class == "PA" | capad_t_class == "PA" ~ "PA",
      TRUE ~ "not protected")
  ) |> 
  select(-c(capad_m_class, capad_t_class, forest2018, forest2013)) |> 
  collect() |> 
  mutate(locationID = 1:n()) |> 
  relocate(locationID)

write_dataset(dataset = distinct_loc,
              path = here("data", "interim", "loc"), 
              format = "parquet")


# 2. distinct_taxon -----
# append GRIIS list to distinct_taxon generated in AWS Athena
# presence in the GRIIS list = introduced, having the Invasive flag = invasive,
# everything else (not on the list) = native

dist_taxon <- fread(here("data", "raw", "distinct_taxon_aws.csv"))

# GRIIS files from DH - not rectangular so DT works better
distribution <- fread(here("data", "external", "dwca-griis-australia-v1.6", "distribution.txt"))
species <- fread(here("data", "external", "dwca-griis-australia-v1.6", "speciesprofile.txt"))
taxa <- fread(here("data", "external", "dwca-griis-australia-v1.6", "taxon-edited.txt"), fill = TRUE)

# create a GRIIS list
griis_list <- taxa |>  
  full_join(distribution, by = "id") |>  
  full_join(species, by = "id")

# look up ALA identifiers based on scientific names in GRIIS list
griis_ala_raw <- as.data.frame(search_taxa(griis_list$scientificName))

# clean up search_taxa() results
griis_ala_tidy <- griis_ala_raw |>  
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) |>  
  select(search_term, scientific_name, taxon_concept_id)

# add ALA identifiers to cleaned up GRIIS list 
# only includes species that match completely with ALA records, to species level etc.
griis <- inner_join(griis_list, 
                    griis_ala_tidy, 
                    by = c("scientificName" = "search_term")) 

# join to distinct_taxon
griis_taxon_joined <- griis |>  
  select(scientific_name, isInvasive) |>  
  right_join(dist_taxon, by = c("scientific_name" = "speciesName")) |>  
  mutate(griisStatus = case_when(
    isInvasive == "Invasive" ~ "Invasive",
    isInvasive == "Null" ~ "Introduced",
    is.na(isInvasive) ~ "Native")) |>  
  select(-isInvasive) |>  
  relocate(speciesName = scientific_name, .after = genus) 

# remove duplicated rows, identify records with multiple status types
# needs to be run every time because name matching through galah can
# produce different results e.g. if the names index is updated
duplicates <- griis_taxon_joined |> 
  distinct() |> 
  group_by(speciesId) |> 
  summarise(count = n()) |> 
  filter(count > 1)

# check status types
filter(griis_taxon_joined, speciesId == as.character(duplicates[1, 1]))

# manually reclassify records with multiple status types
distinct_taxon_griis <- griis_taxon_joined |> 
  distinct() |> 
  filter(!(speciesId == as.character(duplicates[1, 1]) & griisStatus == "Introduced"))

write_dataset(dataset = distinct_taxon_griis,
              path = here("data", "interim", "taxa"),
              format = "parquet")


# 3. grouped_occ ----
# get grouped counts of speciesId, year, locationID, basisOfRecord, epbcStatus
# add epbc status  
# substitute decimalLat and decimalLon for locationID from distinct_loc

occ <- open_dataset(here("data", "raw", "grouped_occ_aws.csv"), format = "csv")

# EPBC list from Cam via DAWE
epbc_list <- read_csv_arrow(here("data", "external", "epbc_20220503.csv"))

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
  select(-c(speciesName, `Threatened status`)) |> 
  compute()

# add locationID to grouped_occ 
loc_subset <- open_dataset("data/interim/loc", format = "parquet") |> 
  select(locationID, decimalLatitude, decimalLongitude) 

grouped_occ <- add_epbc |> 
  left_join(loc_subset, by = c("decimalLatitude", "decimalLongitude")) |>   
  select(-c("decimalLatitude", "decimalLongitude")) |>
  collect() |> 
  distinct()

write_dataset(dataset = grouped_occ,
              path = here("data", "interim", "occ"),
              format = "parquet")
