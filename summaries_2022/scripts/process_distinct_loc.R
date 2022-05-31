
# distinct_loc: decimalLatitude, decimalLongitude, stateTerritory, capadStatus, 
# forest2018Status, forest2013Status, ibraRegion, imcraRegion
# replace "capad_terrestrial" and "capad_marine" with a single capadStatus value, 
# which should be one of {"IPA", "PA", "Not protected"} regardless of whether 
# that comes from the terrestrial or the marine layer
# add a unique numeric key to each row in distinct_loc ("locationID") and 
# use that in grouped_occ instead of the coordinates to make joins simpler

library(arrow)
library(here)
library(dplyr)

# for faster wrangling
ds <- open_dataset(here("summaries_2022", 
                        "data", 
                        "raw",
                        "aws", 
                        "distinct_loc_raw.csv"), 
                   format = "csv")

# get values for fields that need wrangling
forest_2018_vals <- ds |> 
  distinct(forest2018) |> 
  collect()

forest_2013_vals <- ds |> 
  distinct(forest2013) |> 
  collect()

capad_t_vals <- ds |> 
  distinct(capad_t_class) |> 
  collect()

capad_m_vals <- ds |> 
  distinct(capad_m_class) |> 
  collect()

# wrangle!
distinct_loc <- ds |> 
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

write_csv_arrow(distinct_loc, here("summaries_2022", 
                                   "data", 
                                   "processed", 
                                   "distinct_loc.csv"))
