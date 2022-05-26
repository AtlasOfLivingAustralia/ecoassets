
# Changes in forest biodiversity (from 1900, 5-year interval) 
# forest2013_5year and forest2018_5year: 
# yearStart, yearEnd, forest2013Status or forest2018Status, 
# stateTerritory, recordCount, speciesCount
# NB {vroom} seems sufficiently fast here without needing {arrow}

library(here)
library(dplyr)
library(vroom)

occ <- vroom(here("summaries_2022",
                  "data",
                  "processed",
                  "grouped_occ.csv"), 
             col_select = c(year,
                            count,
                            locationID, 
                            speciesId)) |> 
  filter(year <= 2020)

loc <- vroom(here("summaries_2022", 
                  "data", 
                  "processed",
                  "distinct_loc.csv"),
             col_select = c(stateTerritory, 
                            locationID,
                            forest2013Status,
                            forest2018Status)) |> 
  filter(stateTerritory != "Unknown1") |> 
  filter(!is.na(stateTerritory))
  

# forest 2013 ------

# |- join and mutate -----
joined_2013 <- loc |> 
  select(-forest2018Status) |> 
  left_join(occ, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  mutate(period_key = case_when(
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
  
# |- get counts ------
grouped_2013 <- joined_2013 |> 
  group_by(forest2013Status, period_key, stateTerritory) |> 
  summarise(recordCount = sum(count),
             speciesCount = n_distinct(speciesId))

# |- convert time periods into start and end year -----
forest2013_5year <- grouped_2013 |> 
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
  relocate(c(yearStart, yearEnd))

# |- write file -----
vroom_write(forest2013_5year, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "forest2013_5year.csv"),
            delim = ",")


# forest 2018 ------

# |- join and mutate -----
joined_2018 <- loc |> 
  select(-forest2013Status) |> 
  left_join(occ, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  mutate(period_key = case_when(
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

# |- get counts ------
grouped_2018 <- joined_2018 |> 
  group_by(forest2018Status, period_key, stateTerritory) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId))

# |- convert time periods into start and end year -----
forest2018_5year <- grouped_2018 |> 
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
  relocate(c(yearStart, yearEnd))

# |- write file -----
vroom_write(forest2018_5year, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "forest2018_5year.csv"),
            delim = ",")

