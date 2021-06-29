options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "merged/NRM/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
df1 <- plyr::ldply(data, data.table::fread)

# Subsetting data
df1 <- df1 %>%
  dplyr::select(species_guid, year, basisOfRecord, Australian.States.and.Territories,
                IBRA.7.Regions, CAPAD2020_NAME, wons_status, griis_status,
                conservation_status, CM_ZONE, indigenous_Status)

# Renaming some columns
colnames(df1) <- c("species_guid", "year", "basisOfRecord", "States&Territories",
                   "IBRA.7.Regions", "CAPAD2020_NAME", "wons_status", "griis_status",
                   "conservation_status", "ConservManageZone", "indigenous_Status")

# Removing blank cells
df1 <- df1[!(is.na(df1$species_guid) | df1$species_guid == ""),]
df1 <- df1[!(is.na(df1$IBRA.7.Regions) | df1$IBRA.7.Regions == ""),]


# Group years into 5-year ranges
ala_count <- df1 %>%
  dplyr::mutate(YearRange = cut(df1$year, breaks = seq(1980, 2020, 5)))

rm(df1)

levels(ala_count$YearRange) <- paste(
  seq(1980, 2015, 5) + 1,
  seq(1985, 2020, 5),
  sep = "-")

colnames(ala_count)[5] <- c("IBRA")

# Removing blank cells
ala_count <- ala_count[!(is.na(ala_count$YearRange) | ala_count$YearRange == ""),]

occByIbra <- setDT(ala_count)[, .(count = .N), keyby = c("IBRA", "YearRange")]

fwrite(occByIbra, "SumTable/occByIbra.csv")

rm(occByIbra)

sppByIbra <- ala_count %>%
  dplyr::select("species_guid", "IBRA", "YearRange")

# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)

sppByIbra <- setDT(sppByIbra)[, .(count = .N), keyby = c("IBRA", "YearRange")]
fwrite(sppByIbra, "SumTable/sppByIbra.csv")
rm(sppByIbra)

# Number of citizen science records
cs <- ala_count %>%
  dplyr::select(IBRA, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HumanObservation", "CitizenScience", "OtherSource"))


cs <- setDT(cs)[, .(count = .N), keyby = c("IBRA", "YearRange", "basisOfRecord")]
fwrite(cs, "SumTable/NumberOfCitizenScienceRecords.csv")


# Number of citizen science species
cs <- ala_count %>%
  dplyr::select(species_guid, IBRA, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HumanObservation", "CitizenScience", "OtherSource"))

# Removing duplicates
setkey(cs,NULL)
cs <- unique(cs)

cs1 <- setDT(cs)[, .(count = .N), keyby = c("IBRA", "YearRange", "basisOfRecord")]
fwrite(cs1, "SumTable/NumberOfSpecies_RecordedByCitizenScience.csv")

rm(cs, cs1)

# Number of records inside/outside PA
pa <- ala_count %>%
  dplyr::select(species_guid, IBRA, YearRange, CAPAD2020_NAME, 
                indigenous_Status)

pa <- pa %>% 
  dplyr::mutate(pa_status = ifelse(CAPAD2020_NAME == "Outside", "outside", "inside"))

pa1 <- setDT(pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "pa_status", "indigenous_Status")]

fwrite(pa1, "SumTable/NumberOfRecords_PAStatus.csv")

# Removing duplicates
setkey(pa,NULL)
pa1 <- unique(pa)

pa2 <- setDT(pa1)[, .(count = .N), keyby = c("IBRA", "YearRange", "pa_status", "indigenous_Status")]

fwrite(pa2, "SumTable/NumberOfSpecies_PAStaus.csv")

rm(pa, pa1, pa2)

# Species which are either inside or outside PAs
pa <- ala_count %>%
  dplyr::select(species_guid, IBRA, CAPAD2020_NAME, YearRange, indigenous_Status)

pa1 <- pa %>% 
  dplyr::mutate(pa_status = ifelse(CAPAD2020_NAME == "Outside", "outside", "inside"))

pa1 <- pa1 %>%
  dplyr::select(species_guid, IBRA, pa_status, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa1,NULL)
pa1 <- unique(pa1)

pa2 <- setDT(pa1)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange", "pa_status", "indigenous_Status")]

head(pa2)

pa_inside <- pa2 %>%
  dplyr::filter(pa_status == "inside")

pa_outside <- pa2 %>%
  dplyr::filter(pa_status == "outside")

fwrite(pa_inside, "SumTable/SpeciesOnlyInsidePA.csv")

fwrite(pa_outside, "SumTable/SpeciesOnlyOutsidePA.csv")

rm(pa, pa1, pa2, pa_inside, pa_outside)

# Species first/last seen
ala_count_simple <- setDT(ala_count)[,.(.N), c("IBRA", "YearRange", "species_guid")]

ala_count_simple$YearRange <- as.numeric(ala_count_simple$YearRange)
df_final <- ala_count_simple[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  cumulative_species <-length(unique(
    x[IBRA == a$IBRA & YearRange <= a$YearRange, "species_guid"]$species_guid
  ))
  number_of_species <- length(unique(current_year_species))
  return(c(
    species_richness = number_of_species,
    new_species = new_species, 
    last_seen_species = last_seen_species,
    cumulative_species = cumulative_species))
}, x = ala_count_simple)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(8),
  labels = levels(ala_count$YearRange))

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved.csv")
