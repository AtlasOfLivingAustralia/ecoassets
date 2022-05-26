
library(here)
library(dplyr)
library(vroom)

forest2018 <- vroom(here("summaries_2022", "data", "processed", "forest2018_5year.csv"))
forest2013 <- vroom(here("summaries_2022", "data", "processed", "forest2013_5year.csv"))
joined <- full_join(forest2018, forest2013, by = c("yearStart", "yearEnd", "stateTerritory"))
joined_tidy <- joined |> 
  rename("recordCount2018" = "recordCount.x",
         "speciesCount2018" = "speciesCount.x",
         "recordCount2013" = "recordCount.y",
         "speciesCount2013" = "speciesCount.y") |> 
  relocate(stateTerritory, .after = yearEnd)

# DH's comments -------
# 1. Count of records from areas not forested in 2013 or 2018
# 2. Count of records from areas forested in 2013 but not 2018
# 3. Count of records from areas forested in 2018 but not 2013
# 4. Count of records from areas forested in 2013 and 2018
# 5. Count of species from areas not forested in 2013 or 2018
# 6. Count of species from areas forested in 2013 but not 2018
# 7. Count of species from areas forested in 2018 but not 2013
# 8. Count of species from areas forested in 2013 and 2018
         
cat01 <- joined_tidy |> 
  filter(forest2018Status == "non-forest" & forest2013Status == "non-forest")
