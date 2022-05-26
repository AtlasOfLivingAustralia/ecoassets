
# summarise the split between different basisOfRecord values by the same periods 
# as the rest of the datasets (1900-1970, 1971-5, etc.)
# Start year, End year, Region, Specimen count, human observation count,
# Machine observation count,  Unknown observation count 
# Specimen - Preserved specimen or Material Sample or Living specimen
# Human observation - Observation or Human observation
# Machine observation  - Machine observation
# Unknown - Unknown

library(here)
library(vroom)
library(dplyr)
library(tidyr)


occ <- vroom(here("summaries_2022", 
                  "data", 
                  "processed",
                  "grouped_occ.csv"),
             col_select = c(year, 
                            count, 
                            basisOfRecord, 
                            locationID)) |> 
  filter(year <= 2020)

loc <- vroom(here("summaries_2022", 
                  "data", 
                  "processed",
                  "distinct_loc.csv"),
             col_select = c(ibraRegion,
                            imcraRegion, 
                            locationID))


# IBRA -------
# join and mutate into time periods -----
ibra_joined <- loc |> 
  select(-imcraRegion) |> 
  filter(!is.na(ibraRegion)) |> 
  left_join(occ, by = "locationID") |> 
  filter(!is.na(year)) |> 
  mutate(
    period_key = case_when(
      year <= 1970 ~ 1,
      year >= 1971 & year <= 1975 ~ 2,
      year >= 1976 & year <= 1980 ~ 3,
      year >= 1981 & year <= 1985 ~ 4,
      year >= 1986 & year <= 1990 ~ 5,
      year >= 1991 & year <= 1995 ~ 6,
      year >= 1996 & year <= 2000 ~ 7,
      year >= 2001 & year <= 2005 ~ 8,
      year >= 2006 & year <= 2010 ~ 9,
      year >= 2011 & year <= 2015 ~ 10, 
    year >= 2016 & year <= 2020 ~ 11)) |> 
  select(-c(locationID, year))

# get counts ------
# sum each set of counts separately (probably quicker with purrr) 

# specimen counts
specimen_count <- ibra_joined |> 
  filter(basisOfRecord %in% c("PRESERVED_SPECIMEN",
                              "MATERIAL_SAMPLE",
                              "LIVING_SPECIMEN", 
                              "FOSSIL_SPECIMEN")) |> 
  group_by(ibraRegion, period_key) |> 
  summarise(specimenCount = sum(count))

# human observation count
hobs_count <- ibra_joined |> 
  filter(basisOfRecord %in% c("OBSERVATION",
                              "HUMAN_OBSERVATION")) |> 
  group_by(ibraRegion, period_key) |> 
  summarise(humanObservationCount = sum(count))

# machine observation count
mobs_count <- ibra_joined |> 
  filter(basisOfRecord == "MACHINE_OBSERVATION") |> 
  group_by(ibraRegion, period_key) |> 
  summarise(machineObservationCount = sum(count))

# unknown counts
unknown_count <- ibra_joined |> 
  filter(basisOfRecord %in% c("UNKNOWN", "OCCURRENCE")) |> 
  group_by(ibraRegion, period_key) |> 
  summarise(unknownObservationCount = sum(count))

# join and mutate into start and end years ------

ibra_record_type <- specimen_count |> 
  full_join(hobs_count, by = c("ibraRegion", "period_key")) |> 
  full_join(mobs_count, by = c("ibraRegion", "period_key")) |> 
  full_join(unknown_count, by = c("ibraRegion", "period_key")) |> 
  mutate(
    yearStart = case_when(
      period_key == 1 ~ 1900,
      period_key == 2 ~ 1971,
      period_key == 3 ~ 1976,
      period_key == 4 ~ 1981,
      period_key == 5 ~ 1986,
      period_key == 6 ~ 1991,
      period_key == 7 ~ 1996,
      period_key == 8 ~ 2001,
      period_key == 9 ~ 2006,
      period_key == 10 ~ 2011,
      period_key == 11 ~ 2016), 
    yearEnd = case_when(
      period_key == 1 ~ 1970,
      period_key == 2 ~ 1975,
      period_key == 3 ~ 1980,
      period_key == 4 ~ 1985,
      period_key == 5 ~ 1990,
      period_key == 6 ~ 1995,
      period_key == 7 ~ 2000,
      period_key == 8 ~ 2005,
      period_key == 9 ~ 2010,
      period_key == 10 ~ 2015,
      period_key == 11 ~ 2020)) |> 
  ungroup() |>
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = ibraRegion) |> 
  mutate(across(specimenCount:unknownObservationCount, ~replace_na(., 0)))

# write file -----
vroom_write(ibra_record_type, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "ibra_record_type.csv"),
            delim = ",")

  
# IMCRA --------
# join and mutate into time periods -----
imcra_joined <- loc |> 
  select(-ibraRegion) |> 
  filter(!is.na(imcraRegion)) |> 
  left_join(occ, by = "locationID") |> 
  filter(!is.na(year)) |> 
  mutate(
    period_key = case_when(
      year <= 1970 ~ 1,
      year >= 1971 & year <= 1975 ~ 2,
      year >= 1976 & year <= 1980 ~ 3,
      year >= 1981 & year <= 1985 ~ 4,
      year >= 1986 & year <= 1990 ~ 5,
      year >= 1991 & year <= 1995 ~ 6,
      year >= 1996 & year <= 2000 ~ 7,
      year >= 2001 & year <= 2005 ~ 8,
      year >= 2006 & year <= 2010 ~ 9,
      year >= 2011 & year <= 2015 ~ 10, 
      year >= 2016 & year <= 2020 ~ 11)) |> 
  select(-c(locationID, year))

# get counts ------
# sum each set of counts separately (probably quicker with purrr) 

# specimen counts
specimen_count <- imcra_joined |> 
  filter(basisOfRecord %in% c("PRESERVED_SPECIMEN",
                              "MATERIAL_SAMPLE",
                              "LIVING_SPECIMEN", 
                              "FOSSIL_SPECIMEN")) |> 
  group_by(imcraRegion, period_key) |> 
  summarise(specimenCount = sum(count))

# human observation count
hobs_count <- imcra_joined |> 
  filter(basisOfRecord %in% c("OBSERVATION",
                              "HUMAN_OBSERVATION")) |> 
  group_by(imcraRegion, period_key) |> 
  summarise(humanObservationCount = sum(count))

# machine observation count
mobs_count <- imcra_joined |> 
  filter(basisOfRecord == "MACHINE_OBSERVATION") |> 
  group_by(imcraRegion, period_key) |> 
  summarise(machineObservationCount = sum(count))

# unknown counts
unknown_count <- imcra_joined |> 
  filter(basisOfRecord %in% c("UNKNOWN", "OCCURRENCE")) |> 
  group_by(imcraRegion, period_key) |> 
  summarise(unknownObservationCount = sum(count))


# join and mutate into start and end years ------

imcra_record_type <- specimen_count |> 
  full_join(hobs_count, by = c("imcraRegion", "period_key")) |> 
  full_join(mobs_count, by = c("imcraRegion", "period_key")) |> 
  full_join(unknown_count, by = c("imcraRegion", "period_key")) |> 
  mutate(
    yearStart = case_when(
      period_key == 1 ~ 1900,
      period_key == 2 ~ 1971,
      period_key == 3 ~ 1976,
      period_key == 4 ~ 1981,
      period_key == 5 ~ 1986,
      period_key == 6 ~ 1991,
      period_key == 7 ~ 1996,
      period_key == 8 ~ 2001,
      period_key == 9 ~ 2006,
      period_key == 10 ~ 2011,
      period_key == 11 ~ 2016), 
    yearEnd = case_when(
      period_key == 1 ~ 1970,
      period_key == 2 ~ 1975,
      period_key == 3 ~ 1980,
      period_key == 4 ~ 1985,
      period_key == 5 ~ 1990,
      period_key == 6 ~ 1995,
      period_key == 7 ~ 2000,
      period_key == 8 ~ 2005,
      period_key == 9 ~ 2010,
      period_key == 10 ~ 2015,
      period_key == 11 ~ 2020)) |> 
  ungroup() |>
  select(-period_key) |> 
  relocate(c(yearStart, yearEnd), .after = imcraRegion) |> 
  mutate(across(specimenCount:unknownObservationCount, ~replace_na(., 0)))

# write file -----
vroom_write(imcra_record_type, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "imcra_record_type.csv"),
            delim = ",")

