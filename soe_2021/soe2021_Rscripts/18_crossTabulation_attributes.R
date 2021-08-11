# Loading libraries
library(data.table)
library(dplyr)
library(galah)

# Cross-checking GRIIS dataset
griis_list <- fread("cache/otherAttributes/griis.csv")
griis_ala_original <- select_taxa(griis_list$species)
griis <- griis_list %>% 
  dplyr::left_join(griis_ala_original, by = c("scientificName" = "scientific_name"))
fwrite(griis, "cache/griis_ala.csv")

# Cross-checking WoNS dataset
wons_list <- fread("cache/otherAttributes/wons.csv")
wons_ala_original <- select_taxa(wons_list$scientificName)
wons <- wons_list %>% 
  dplyr::left_join(wons_ala_original, by = c("scientificName" = "scientific_name"))
fwrite(wons, "cache/wons_ala.csv")


# Cross-checking EPBC list
threatened_list <- fread("cache/otherAttributes/epbc.csv")
threatened_ala_original <- select_taxa(threatened_list$scientific_name)
epbc <- threatened_list %>% 
  dplyr::left_join(threatened_ala_original, by = c("scientific_name"))
fwrite(epbc, "cache/epbc_ala.csv")
