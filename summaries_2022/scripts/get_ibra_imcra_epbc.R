
# Changes in EPBC listed species by IBRA & IMCRA region (from 1900, 5-year interval) 
# ibra_epbc_5year and imcra_epbc_5year: 
# ibra or imcra, yearStart, yearEnd, epbcStatus, recordCount, speciesCount
# NB {vroom} seems sufficiently fast here without needing {arrow}

library(here)
library(dplyr)
library(vroom)


occ <- vroom(here("summaries_2022", 
                         "data", 
                         "processed",
                         "grouped_occ.csv"),
                    col_select = c(speciesId, 
                                   year, 
                                   locationID, 
                                   epbcStatus, 
                                   count)) |> 
  filter(year <= 2020)

# IBRA -------
loc_ibra <- vroom(here("summaries_2022", 
                                "data", 
                                "processed",
                                "distinct_loc.csv"),
                           col_select = c(locationID, 
                                          ibraRegion)) |> 
  filter(!is.na(ibraRegion))

# |- code years into time periods for summarising -----
occ_loc_ibra <- loc_ibra |> 
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
grouped_ibra <- occ_loc_ibra |> 
  group_by(ibraRegion, period_key, epbcStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId))
  
# |- convert time periods into start and end year -----
ibra_epbc_5year <- grouped_ibra |> 
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
  relocate(c(yearStart, yearEnd), .after = ibraRegion)

# |- write file -----
vroom_write(ibra_epbc_5year, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "ibra_epbc_5year.csv"),
            delim = ",")


# IMCRA -------
loc_imcra <- vroom(here("summaries_2022", 
                       "data", 
                       "processed",
                       "distinct_loc.csv"),
                  col_select = c(locationID, 
                                 imcraRegion)) |> 
  filter(!is.na(imcraRegion))

# |- code years into time periods for summarising -----
occ_loc_imcra <- loc_imcra |> 
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
grouped_imcra <- occ_loc_imcra |> 
  group_by(imcraRegion, period_key, epbcStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId))

# |- convert time periods into start and end year -----
imcra_epbc_5year <- grouped_imcra |> 
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
  relocate(c(yearStart, yearEnd), .after = imcraRegion)

# |- write file -----
vroom_write(imcra_epbc_5year, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "imcra_epbc_5year.csv"),
            delim = ",")

