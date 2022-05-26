
# Representativeness of biodiversity in protected areas by IBRA & IMCRA region 
# capad_ibra and capad_imcra: ibraRegion or imcraRegion, epbcStatus, capadStatus, 
# recordCount, speciesCount
# capad_ibra_species and capad_imcra_species: ibraRegion or imcraRegion, 
# speciesID, speciesName, epbcStatus regionRecordCount, protectedRecordCount, 
# indigenousProtectedRecordCount
# regionRecordCount = all records regardless of CAPAD status, 
# protectedRecordCount = all PA or IPA records, 
# indigenousProtectedRecordCount = IPA records

library(here)
library(dplyr)
library(vroom)


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

# capad_ibra ------

capad_ibra <- loc |> 
  filter(!is.na(ibraRegion)) |> 
  select(-imcraRegion) |> 
  left_join(occ, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  group_by(ibraRegion, epbcStatus, capadStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId))

vroom_write(capad_ibra, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "capad_ibra.csv"),
            delim = ",")


# capad imcra -----

capad_imcra <- loc |> 
  filter(!is.na(imcraRegion)) |> 
  select(-ibraRegion) |> 
  left_join(occ, by = "locationID") |> 
  filter(!is.na(speciesId)) |> 
  group_by(imcraRegion, epbcStatus, capadStatus) |> 
  summarise(recordCount = sum(count),
            speciesCount = n_distinct(speciesId))

vroom_write(capad_imcra, 
            here("summaries_2022",
                 "data",
                 "processed",
                 "capad_imcra.csv"),
            delim = ",")



