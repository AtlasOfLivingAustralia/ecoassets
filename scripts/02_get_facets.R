
# get 6 facets from processed AWS datasets
# ADD SOMETHING WITH {POINTBLANK} AFTER GENERATING EACH FACET AND WRITING TO CSV

library(here)
library(dplyr)
library(arrow)
library(tidyr)
library(vroom) 

source(here("scripts", "functions.R"))

loc <- open_dataset(here("data", "interim", "loc"), 
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
occ <- open_dataset(here("data", "interim", "occ"), 
                    format = "parquet",
                    schema = schema(speciesId = large_utf8(), 
                                    year = int32(),
                                    basisOfRecord = string(),
                                    count = int32(),
                                    epbcStatus = string(),
                                    locationID = int32()))
taxa <- open_dataset(here("data", "interim", "taxa"), 
                     format = "parquet", 
                     schema = schema(speciesId = large_utf8(),
                                     kingdom = string(),
                                     phylum = string(),
                                     class = string(),
                                     order = string(),
                                     family = string(),
                                     genus = string(),
                                     speciesName = string(),
                                     griisStatus = string()))

# 01. ibra_imcra_griis -----
# changes in invasive species by IBRA & IMCRA (from 1900, 5-year interval)

taxa_subset <- taxa |> 
  select(speciesId,
         griisStatus) |> 
  collect()

occ_taxa <- occ |> 
  select(year,
         count, 
         speciesId,
         locationID) |> 
  filter(year <= 2020) |> 
  left_join(taxa_subset, by = "speciesId") |> 
  collect()

# IBRA
loc |> 
  select(ibraRegion,
         locationID) |> 
  filter(ibraRegion != "") |> 
  collect() |> 
  left_join(occ_taxa, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  as_period() |> 
  select(-c(locationID, year)) |> 
  group_by(ibraRegion, period_key, griisStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |> 
  as_start_end() |> 
  ungroup() |> 
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = ibraRegion) |> 
  vroom_write(here("data", "processed", "ibra_griis_5year.csv"), delim = ", ")

# IMCRA
loc |> 
  select(imcraRegion,
         locationID) |> 
  filter(imcraRegion != "") |> 
  collect() |> 
  left_join(occ_taxa, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  as_period() |> 
  select(-c(locationID, year)) |> 
  group_by(imcraRegion, period_key, griisStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |> 
  as_start_end() |> 
  ungroup() |> 
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = imcraRegion) |> 
  vroom_write(here("data", "processed", "imcra_griis_5year.csv"), delim = ",")


# 02. ibra_imcra_epbc ------
# changes in EPBC-listed species by IBRA & IMCRA (from 1900, 5-year interval)

occ_subset <- occ |> 
  select(speciesId, 
         year, 
         locationID, 
         epbcStatus, 
         count) |> 
  filter(year <= 2020) |> 
  collect()

# IBRA 
loc |> 
  select(ibraRegion, 
         locationID) |> 
  filter(ibraRegion != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  as_period() |> 
  select(-c(locationID, year)) |>
  group_by(ibraRegion, period_key, epbcStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |>
  as_start_end() |> 
  ungroup() |> 
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = ibraRegion) |> 
  vroom_write(here("data", "processed", "ibra_epbc_5year.csv"), delim = ",")
  
# IMCRA
loc |> 
  select(imcraRegion, 
         locationID) |> 
  filter(imcraRegion != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  as_period() |> 
  select(-c(locationID, year)) |>
  group_by(imcraRegion, period_key, epbcStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |>
  as_start_end() |> 
  ungroup() |> 
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = imcraRegion) |> 
  vroom_write(here("data", "processed", "imcra_epbc_5year.csv"), delim = ",")


# 03. capad_ibra_imcra ------
# representativeness of biodiversity in protected areas by IBRA & IMCRA 

occ_subset <- occ |> 
  select(epbcStatus, 
         count, 
         locationID,
         speciesId) |> 
  collect()

# IBRA
loc |> 
  select(locationID, 
         ibraRegion, 
         capadStatus) |> 
  filter(ibraRegion != "") |> 
  collect() |>
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  group_by(ibraRegion, epbcStatus, capadStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |> 
  vroom_write(here("data", "processed", "capad_ibra.csv"), delim = ",")

# IMCRA
loc |> 
  select(locationID, 
         imcraRegion, 
         capadStatus) |> 
  filter(imcraRegion != "") |> 
  collect() |>
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  group_by(imcraRegion, epbcStatus, capadStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |> 
  vroom_write(here("data", "processed", "capad_imcra.csv"), delim = ",")
  

# 04. capad_ibra_imcra_species ------
# records of species in areas with different protection status

occ_subset <- occ |> 
  select(epbcStatus, 
         count,
         locationID,
         speciesId) |> 
  collect()

taxa_subset <- taxa |>
  select(speciesName,
         speciesId) |> 
  collect()

# IBRA
ibra_joined <- loc |> 
  select(locationID,
         ibraRegion,
         capadStatus) |> 
  filter(ibraRegion != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  select(-locationID) |> 
  left_join(taxa_subset, by = "speciesId") |> 
  filter(!is.na(speciesId))

# regionRecordCount: all records regardless of CAPAD status
region_count_ibra <- ibra_joined |> 
  group_by(speciesName, ibraRegion) |>
  summarise(regionRecordCount = sum(count))

# protectedRecordCount: all PA or IPA records 
protected_count_ibra <- ibra_joined |> 
  filter(capadStatus %in% c("PA", "IPA")) |> 
  group_by(speciesName, ibraRegion) |>
  summarise(protectedRecordCount = sum(count))

# indigenousProtectedRecordCount: IPA records
ipa_count_ibra <- ibra_joined |> 
  filter(capadStatus == "IPA") |> 
  group_by(speciesName, ibraRegion) |>
  summarise(indigenousProtectedRecordCount = sum(count))

ibra_joined |> 
  left_join(region_count_ibra, by = c("speciesName", "ibraRegion")) |> 
  left_join(protected_count_ibra, by = c("speciesName", "ibraRegion")) |> 
  left_join(ipa_count_ibra, by = c("speciesName", "ibraRegion")) |> 
  select(-c(capadStatus, count)) |> 
  relocate(c(speciesId, speciesName), .before = epbcStatus) |> 
  mutate(protectedRecordCount = replace_na(protectedRecordCount, 0),
         indigenousProtectedRecordCount= replace_na(indigenousProtectedRecordCount, 0)) |> 
  vroom_write(here("data", "processed", "capad_ibra_species.csv"), delim = ",")

# IMCRA
imcra_joined <- loc |> 
  select(locationID,
         imcraRegion,
         capadStatus) |> 
  filter(imcraRegion != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  select(-locationID) |> 
  left_join(taxa_subset, by = "speciesId") |> 
  filter(!is.na(speciesId))

# regionRecordCount: all records regardless of CAPAD status
region_count_imcra <- imcra_joined |> 
  group_by(speciesName, imcraRegion) |>
  summarise(regionRecordCount = sum(count))

# protectedRecordCount: all PA or IPA records 
protected_count_imcra <- imcra_joined |> 
  filter(capadStatus %in% c("PA", "IPA")) |> 
  group_by(speciesName, imcraRegion) |>
  summarise(protectedRecordCount = sum(count))

# indigenousProtectedRecordCount: IPA records
ipa_count_imcra <- imcra_joined |> 
  filter(capadStatus == "IPA") |> 
  group_by(speciesName, imcraRegion) |>
  summarise(indigenousProtectedRecordCount = sum(count))

imcra_joined |> 
  left_join(region_count_imcra, by = c("speciesName", "imcraRegion")) |> 
  left_join(protected_count_imcra, by = c("speciesName", "imcraRegion")) |> 
  left_join(ipa_count_imcra, by = c("speciesName", "imcraRegion")) |> 
  select(-c(capadStatus, count)) |> 
  relocate(c(speciesId, speciesName), .before = epbcStatus) |> 
  mutate(protectedRecordCount = replace_na(protectedRecordCount, 0),
         indigenousProtectedRecordCount= replace_na(indigenousProtectedRecordCount, 0)) |> 
  vroom_write(here("data", "processed", "capad_imcra_species.csv"), delim = ",")


# 05. forest_biodiv ------
# changes in forest biodiversity (from 1900, 5-year interval) 

occ_subset <- occ |> 
  select(year,
         count,
         locationID,
         speciesId) |>
  filter(year <= 2020) |> 
  collect()

# forest2013
loc |> 
  select(stateTerritory, 
         locationID,
         forest2013Status) |> 
  filter(stateTerritory != "Unknown1") |> 
  filter(stateTerritory != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  as_period() |> 
  select(-c(locationID, year)) |> 
  group_by(forest2013Status, period_key, stateTerritory) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |> 
  as_start_end() |> 
  ungroup() |> 
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd)) |> 
  vroom_write(here("data", "processed", "forest2013_5year.csv"), delim = ",")

# forest2018
loc |> 
  select(stateTerritory, 
         locationID,
         forest2018Status) |> 
  filter(stateTerritory != "Unknown1") |> 
  filter(stateTerritory != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  as_period() |> 
  select(-c(locationID, year)) |> 
  group_by(forest2018Status, period_key, stateTerritory) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId)) |> 
  as_start_end() |> 
  ungroup() |> 
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd)) |> 
  vroom_write(here("data", "processed", "forest2018_5year.csv"), delim = ",")


# 06. ibra_imcra_recordtype ------
# record counts by different basisOfRecord values (from 1900, 5-year interval) 

occ_subset <- occ |> 
  select(year,
         count, 
         basisOfRecord,
         locationID) |> 
  filter(year <= 2020) |> 
  collect()

# IBRA
ibra_joined <- loc |> 
  select(ibraRegion, 
         locationID) |> 
  filter(ibraRegion != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(year)) |> 
  as_period() |> 
  select(-c(locationID, year))

# specimen counts
spec_count_ibra <- ibra_joined |> 
  filter(basisOfRecord %in% c("PRESERVED_SPECIMEN",
                              "MATERIAL_SAMPLE",
                              "LIVING_SPECIMEN", 
                              "FOSSIL_SPECIMEN")) |> 
  group_by(ibraRegion, period_key) |> 
  summarise(specimenCount = sum(count))

# human observation count
hobs_count_ibra <- ibra_joined |> 
  filter(basisOfRecord %in% c("OBSERVATION",
                              "HUMAN_OBSERVATION")) |> 
  group_by(ibraRegion, period_key) |> 
  summarise(humanObservationCount = sum(count))

# machine observation count
mobs_count_ibra <- ibra_joined |> 
  filter(basisOfRecord == "MACHINE_OBSERVATION") |> 
  group_by(ibraRegion, period_key) |> 
  summarise(machineObservationCount = sum(count))

# unknown counts
unknown_count_ibra <- ibra_joined |> 
  filter(basisOfRecord %in% c("UNKNOWN", "OCCURRENCE")) |> 
  group_by(ibraRegion, period_key) |> 
  summarise(unknownObservationCount = sum(count))

spec_count_ibra |> 
  full_join(hobs_count_ibra, by = c("ibraRegion", "period_key")) |> 
  full_join(mobs_count_ibra, by = c("ibraRegion", "period_key")) |> 
  full_join(unknown_count_ibra, by = c("ibraRegion", "period_key")) |> 
  as_start_end() |> 
  ungroup() |>
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = ibraRegion) |> 
  mutate(across(specimenCount:unknownObservationCount, ~replace_na(., 0))) |> 
  vroom_write(here("data", "processed", "ibra_record_type.csv"), delim = ",")

# IMCRA
imcra_joined <- loc |> 
  select(imcraRegion, 
         locationID) |> 
  filter(imcraRegion != "") |> 
  collect() |> 
  left_join(occ_subset, by = "locationID") |> 
  filter(!is.na(year)) |> 
  as_period() |> 
  select(-c(locationID, year))

# specimen counts
spec_count_imcra <- imcra_joined |> 
  filter(basisOfRecord %in% c("PRESERVED_SPECIMEN",
                              "MATERIAL_SAMPLE",
                              "LIVING_SPECIMEN", 
                              "FOSSIL_SPECIMEN")) |> 
  group_by(imcraRegion, period_key) |> 
  summarise(specimenCount = sum(count))

# human observation count
hobs_count_imcra <- imcra_joined |> 
  filter(basisOfRecord %in% c("OBSERVATION",
                              "HUMAN_OBSERVATION")) |> 
  group_by(imcraRegion, period_key) |> 
  summarise(humanObservationCount = sum(count))

# machine observation count
mobs_count_imcra <- imcra_joined |> 
  filter(basisOfRecord == "MACHINE_OBSERVATION") |> 
  group_by(imcraRegion, period_key) |> 
  summarise(machineObservationCount = sum(count))

# unknown counts
unknown_count_imcra <- imcra_joined |> 
  filter(basisOfRecord %in% c("UNKNOWN", "OCCURRENCE")) |> 
  group_by(imcraRegion, period_key) |> 
  summarise(unknownObservationCount = sum(count))

spec_count_imcra |> 
  full_join(hobs_count_imcra, by = c("imcraRegion", "period_key")) |> 
  full_join(mobs_count_imcra, by = c("imcraRegion", "period_key")) |> 
  full_join(unknown_count_imcra, by = c("imcraRegion", "period_key")) |> 
  as_start_end() |> 
  ungroup() |>
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = imcraRegion) |> 
  mutate(across(specimenCount:unknownObservationCount, ~replace_na(., 0))) |> 
  vroom_write(here("data", "processed", "imcra_record_type.csv"), delim = ",")
