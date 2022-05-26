
# Representativeness of biodiversity in protected areas by IBRA & IMCRA region 
# capad_ibra_species and capad_imcra_species: ibraRegion or imcraRegion, 
# speciesID, speciesName, epbcStatus, regionRecordCount, protectedRecordCount, 
# indigenousProtectedRecordCount
# regionRecordCount = all records regardless of CAPAD status, 
# protectedRecordCount = all PA or IPA records, 
# indigenousProtectedRecordCount = IPA records

library(here)
library(dplyr)
library(vroom)
library(tidyr)


occ <- vroom(here("summaries_2022", 
                  "data", 
                  "processed",
                  "grouped_occ.csv"),
             col_select = c(epbcStatus, 
                            count, 
                            locationID,
                            speciesId))

loc <- vroom(here("summaries_2022", 
                  "data", 
                  "processed",
                  "distinct_loc.csv"),
             col_select = c(locationID,
                            ibraRegion,
                            imcraRegion,
                            capadStatus))

taxa <- vroom(here("summaries_2022", 
                  "data", 
                  "processed",
                  "distinct_taxon_griis.csv"),
             col_select = c(speciesName,
                            speciesId))

# capad_ibra_species -----
# keep this as reference table for final join

ibra_joined <- loc |> 
  select(-imcraRegion) |> 
  filter(!is.na(ibraRegion)) |> 
  left_join(occ, by = "locationID") |> 
  select(-locationID) |> 
  left_join(taxa, by = "speciesId") |> 
  filter(!is.na(speciesId))

# regionRecordCount = all records regardless of CAPAD status ------
# there's probably a better way to do this with tidyr but nothing occurs to me now

add_region_count <- ibra_joined |> 
  group_by(speciesName, ibraRegion) |>
  summarise(regionRecordCount = sum(count))
  
# protectedRecordCount = all PA or IPA records ------

add_protected_count <- ibra_joined |> 
  filter(capadStatus %in% c("PA", "IPA")) |> 
  group_by(speciesName, ibraRegion) |>
  summarise(protectedRecordCount = sum(count))

# indigenousProtectedRecordCount = IPA records ------

add_ipa_count <- ibra_joined |> 
  filter(capadStatus == "IPA") |> 
  group_by(speciesName, ibraRegion) |>
  summarise(indigenousProtectedRecordCount = sum(count))

# join everything ------
# this is just inelegant :\

capad_ibra_species <- ibra_joined |> 
  left_join(add_region_count, by = c("speciesName", "ibraRegion")) |> 
  left_join(add_protected_count, by = c("speciesName", "ibraRegion")) |> 
  left_join(add_ipa_count, by = c("speciesName", "ibraRegion")) |> 
  select(-c(capadStatus, count)) |> 
  relocate(c(speciesId, speciesName), .before = epbcStatus) |> 
  mutate(protectedRecordCount = replace_na(protectedRecordCount, 0),
         indigenousProtectedRecordCount= replace_na(indigenousProtectedRecordCount, 0))

# write file -----
vroom_write(capad_ibra_species, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "capad_ibra_species.csv"),
            delim = ",")             



# capad_imcra_species -----
# reference table for final join

imcra_joined <- loc |> 
  select(-ibraRegion) |> 
  filter(!is.na(imcraRegion)) |> 
  left_join(occ, by = "locationID") |> 
  select(-locationID) |> 
  left_join(taxa, by = "speciesId") |> 
  filter(!is.na(speciesId))

# regionRecordCount = all records regardless of CAPAD status ------

add_region_count <- imcra_joined |> 
  group_by(speciesName, imcraRegion) |>
  summarise(regionRecordCount = sum(count))

# protectedRecordCount = all PA or IPA records ------

add_protected_count <- imcra_joined |> 
  filter(capadStatus %in% c("PA", "IPA")) |> 
  group_by(speciesName, imcraRegion) |>
  summarise(protectedRecordCount = sum(count))

# indigenousProtectedRecordCount = IPA records ------

add_ipa_count <- imcra_joined |> 
  filter(capadStatus == "IPA") |> 
  group_by(speciesName, imcraRegion) |>
  summarise(indigenousProtectedRecordCount = sum(count))

# join everything ------

capad_imcra_species <- imcra_joined |> 
  left_join(add_region_count, by = c("speciesName", "imcraRegion")) |> 
  left_join(add_protected_count, by = c("speciesName", "imcraRegion")) |> 
  left_join(add_ipa_count, by = c("speciesName", "imcraRegion")) |> 
  select(-c(capadStatus, count)) |> 
  relocate(c(speciesId, speciesName), .before = epbcStatus) |> 
  mutate(protectedRecordCount = replace_na(protectedRecordCount, 0),
         indigenousProtectedRecordCount= replace_na(indigenousProtectedRecordCount, 0))

# write file -----
vroom_write(capad_imcra_species, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "capad_imcra_species.csv"),
            delim = ",")             



