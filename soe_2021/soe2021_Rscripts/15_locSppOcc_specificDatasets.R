library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/merged/terrestrial/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

rm(data, input_folder)

# Removing blank cells
ala <- ala[!(is.na(ala$speciesID) | ala$speciesID == ""),]
ala <- ala[!(is.na(ala$IBRA) | ala$IBRA == ""),]
ala <- ala[!(is.na(ala$IMCRA) | ala$IMCRA == ""),]

# Removing duplicates
ala <- setDT(ala)[, .(count = .N), keyby = c("speciesID", "year", "class", "decimalLatitude", "decimalLongitude", "basisOfRecord", "IBRA", "IMCRA", "NRM",
                                             "CAPAD2020_NAME", "CAPAD2020_TYPE", "CAPAD_Status", "indigenous_Status", "isInvasive", "griis_status",
                                             "conservation_status", "date_effective", "epbc", "WoNS", "wons_status")]
# Removing 'count' column
ala <- ala %>% 
  dplyr::select(- count)

# Adding epbc status
ala <- ala %>%
  dplyr::mutate(epbc_status = ifelse(conservation_status == "Non-threatened", 
                                     "Non-threatened", "epbc"))
# Revising GRIIS status
ala <- ala %>% 
  mutate(griis_status = replace(griis_status, griis_status == "NULL", "Introduced but not invasive"),
         griis_status = replace(griis_status, griis_status == "INVASIVE", "Invasive"),
         griis_status = replace(griis_status, griis_status == "other", "None"))

######################################
# Creating the location specific dataset 
# Reading the provided list
loc_count <- fread("cache/donald/location_count.csv")

# Selecting required columns
loc_count_att <- ala %>% 
  dplyr::select("decimalLongitude", "decimalLatitude", "IBRA", "IMCRA", "NRM",
                "CAPAD_Status", "indigenous_Status")

# Removing duplicates
loc_count_att <- setDT(loc_count_att)[, .(count = .N), keyby = c("decimalLongitude", "decimalLatitude", "IBRA", "IMCRA", "NRM",
                                                               "CAPAD_Status", "indigenous_Status")]
# Joining with the provided spreadsheet
loc_count_att <- loc_count_att %>% 
  dplyr::left_join(loc_count, by = c("count", "decimalLongitude", "decimalLatitude", "IBRA", "IMCRA", "NRM"))


# Saving the output
fwrite(loc_count_att, "cache/location_count_new.csv")

# Clearing memory
rm(loc_count, loc_count_att)

###################################
# Creating the species specific dataset 
# Reading the provided list
sp_count <- fread("cache/donald/species_count.csv")

# Joining with the provided spreadsheet
sp_count_att <- ala %>% 
  dplyr::left_join(sp_count, by = c("speciesID", "class"))

# Selecting required columns
sp_count_att <- sp_count_att %>% 
  dplyr::select("count", "speciesID", "kingdom", "phylum", "class", "order", "family", "genus", "species", "griis_status", "wons_status", "epbc_status")

# Removing duplicates
sp_count_att <- setDT(sp_count_att)[, .(count = .N), keyby = c("count", "speciesID", "kingdom", "phylum", 
                                                               "class", "order", "family", "genus", "species", "griis_status", "wons_status", "epbc_status")]

sp_count_att <- sp_count_att[!(is.na(sp_count_att$species) | sp_count_att$species == ""),]


# Saving the output
fwrite(sp_count_att, "cache/species_count_new.csv")

# Clearing memory
rm(sp_count, sp_count_att)

###################################
# Creating the occurrence specific dataset 
# Reading the provided list
occ_count <- fread("cache/donald/occurrence_count.csv")

# Joining with the provided spreadsheet
occ_count_att <- ala %>% 
  dplyr::left_join(occ_count, by = c("speciesID", "year", "decimalLatitude", "decimalLongitude"))

# Selecting required columns
occ_count_att <- occ_count_att %>% 
  dplyr::select("speciesID", "year", "decimalLatitude", "decimalLongitude", "CAPAD_Status", 
                "indigenous_Status", "griis_status", "wons_status", "epbc_status")

# Removing duplicates
occ_count_att <- setDT(occ_count_att)[, .(count = .N), keyby = c("speciesID", "year", "decimalLatitude", "decimalLongitude", "CAPAD_Status", 
                                                                 "indigenous_Status", "griis_status", "wons_status", "epbc_status")]
# Removing 'count' column
occ_count_att <- occ_count_att %>% 
  dplyr::select(- count)

# Saving the output
fwrite(occ_count_att, "cache/occ_count_new.csv")

# Clearing memory
rm(occ_count, occ_count_att)
