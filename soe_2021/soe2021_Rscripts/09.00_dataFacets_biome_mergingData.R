options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/merged/biome/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

# colnames(ala)[9] <- c("CM_Zone")

# Subsetting data
ala <- ala %>%
  dplyr::select(speciesID, year, basisOfRecord,
                B_NAME, CAPAD_Status, wons_status, griis_status,
                conservation_status, NRM, indigenous_Status)

# Renaming some columns
colnames(ala) <- c("species_guid", "year", "basisOfRecord",
                   "biome", "capad_status", "wons_status", "griis_status",
                   "conservation_status", "ConservManageZone", "indigenous_Status")

# Removing blank cells
ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]
ala <- ala[!(is.na(ala$biome) | ala$biome == ""),]

# Total records: 66,675,031

# Group years into 5-year ranges
ala <- ala %>%
  dplyr::mutate(YearRange = cut(ala$year, breaks = c(1900, seq(1980, 2020, 5))))

levels(ala$YearRange) <- c("1900-1980" ,paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-"))


# Removing blank cells
ala <- ala[!(is.na(ala$YearRange) | ala$YearRange == ""),]

ala <- ala %>% 
  dplyr::select(species_guid, YearRange, biome, basisOfRecord, capad_status, indigenous_Status, wons_status, griis_status, conservation_status)
