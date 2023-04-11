# 3/3: grouped occ
# third of 3 scripts that separate downloaded records into 3 relational tables 
# grouped counts of speciesID, year, location, basisOfRecord, epbcStatus 
# appends EPBC status to taxa

ds <- open_dataset("data/galah", format = "parquet")

occ <- ds |> 
  select(speciesID,
         speciesName = species, 
         year,
         decimalLatitude,
         decimalLongitude,
         basisOfRecord,
         cl966,
         cl1048) |> 
  filter(!is.na(cl966) | !is.na(cl1048)) |>
  group_by(speciesID,
           speciesName,
           year,
           decimalLatitude,
           decimalLongitude,
           basisOfRecord) |> 
  summarise(count = n()) |> 
  compute()

# EPBC list from Cam (via DAWE)
epbc_list_cam <- read_csv(here("data",
                               "external",
                               "epbc_20220503.csv"))














