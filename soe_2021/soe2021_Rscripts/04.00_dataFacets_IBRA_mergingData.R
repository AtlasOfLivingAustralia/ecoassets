options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/merged/terrestrial/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

colnames(ala)[9] <- c("CM_Zone")

# Subsetting data
ala <- ala %>%
  dplyr::select(speciesID, year, basisOfRecord,
                IBRA, CAPAD_Status, wons_status, griis_status,
                conservation_status, CM_Zone, indigenous_Status)

# Renaming some columns
colnames(ala) <- c("species_guid", "year", "basisOfRecord",
                   "IBRA", "capad_status", "wons_status", "griis_status",
                   "conservation_status", "ConservManageZone", "indigenous_Status")

# Removing blank cells
ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]
ala <- ala[!(is.na(ala$IBRA) | ala$IBRA == ""),]

# Grouping years into two different types: i) records in between 1900 and 1980 and  5-year ranges afterwards (from 1981)
ala <- ala %>%
  dplyr::mutate(YearRange = cut(ala$year, breaks = c(1900, seq(1980, 2020, 5))))

levels(ala$YearRange) <- c("1900-1980" ,paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-"))

# Removing blank cells
ala <- ala[!(is.na(ala$YearRange) | ala$YearRange == ""),]

