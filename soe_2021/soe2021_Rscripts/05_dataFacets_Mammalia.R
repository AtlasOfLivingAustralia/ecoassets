options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/Merged/Terrestrial/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

colnames(ala)[18] <- c("CM_Zone")

# Subsetting data
ala <- ala %>%
  dplyr::filter(class == "Mammalia")

ala <- ala %>%
  dplyr::select(species_guid, year, basisOfRecord,
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


# Occurrence records count
occByIbra <- ala %>%
  dplyr::select("IBRA", "YearRange")
occByIbra <- setDT(occByIbra)[, .(occByIbra = .N), keyby = c("IBRA", "YearRange")]

fwrite(occByIbra, "SumTable/occByIbra.csv")

rm(occByIbra)

# Species count
sppByIbra <- ala %>%
  dplyr::select("species_guid", "IBRA", "YearRange")

# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)

sppByIbra <- setDT(sppByIbra)[, .(sppByIbra = .N), keyby = c("IBRA", "YearRange")]
fwrite(sppByIbra, "SumTable/sppByIbra.csv")
rm(sppByIbra)

# Number of citizen science records
cs <- ala %>%
  dplyr::select(IBRA, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HumanObservation", "CitizenScience", "OtherSource"))
cs <- cs %>%
  dplyr::filter(basisOfRecord == "CitizenScience")


cs <- setDT(cs)[, .(CS_records = .N), keyby = c("IBRA", "YearRange", "basisOfRecord")]
cs <- cs %>%
  dplyr::select(IBRA, YearRange, CS_records)

fwrite(cs, "SumTable/NumberOfCitizenScienceRecords.csv")
rm(cs)

# Species first/last seen count
ala_simple <- setDT(ala)[,.(.N), c("IBRA", "YearRange", "species_guid")]

ala_simple$YearRange <- as.numeric(ala_simple$YearRange)
df_final <- ala_simple[, .(.N), keyby = c("IBRA", "YearRange")]


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
}, x = ala_simple)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved.csv")

rm(ala_simple, df_final, df_list, result_df, result_list, data, input_folder)

# Number of species inside indigeneous PA
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status")]

sppInPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppInPa)[3] <- "sppInIndPa"

sppOutPa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOutPa <- sppOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOutPa)[3] <- "sppOutIndPa"

fwrite(sppInPa, "SumTable/sppInIndPa.csv")
fwrite(sppOutPa, "SumTable/sppOutIndPa.csv")

rm(pa, sppInPa, sppOutPa)

# Species inside indigenous pa first/last seen count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "sppInIndPa_new_species",
                       "sppInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppInIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species inside PA count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(sppInPa = .N), keyby = c("IBRA", "YearRange", "capad_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, sppInPa)

fwrite(sppInPa, "SumTable/sppInPa.csv")

rm(pa, sppInPa, sppOutPa)

# Species inside PA first/last seen count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "sppInPa_new_species",
                       "sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species which are either inside (not outside) or outside (not inside) PAs
pa <- ala %>%
  dplyr::select(species_guid, IBRA, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status")]

sppOnlyInPa <- pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "sppOnlyInPa"

sppOnlyOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "sppOnlyOutPa"


fwrite(sppOnlyInPa, "SumTable/sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/sppOnlyOutPa.csv")

rm(pa, sppOnlyInPa, sppOnlyOutPa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "sppOnlyInPa_new_species",
                       "sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppOnlyInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "sppOnlyOutPa_new_species",
                       "sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppOnlyOutPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species which are either inside (not outside) or outside (not inside) indigeneous PAs
Ind_pa <- ala %>%
  dplyr::select(species_guid, IBRA, YearRange, indigenous_Status)

# Removing duplicates
setkey(Ind_pa,NULL)
Ind_pa <- unique(Ind_pa)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status")]

head(Ind_pa)

sppOnlyInIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppOnlyOutIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "outside")

fwrite(sppOnlyInIndPa, "SumTable/sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "SumTable/sppOnlyOutIndPa.csv")

rm(Ind_pa, sppOnlyInIndPa, sppOnlyOutIndPa)

# Species distributed only inside (not outside) indigenous PA first/last seen count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "sppOnlyInIndPa_new_species",
                       "sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppOnlyInIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(IBRA, species_guid, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "sppOnlyOutIndPa_new_species",
                       "sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppOnlyOutIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

#############################################################################
# Number of introduced species by IBRA regions
sppByIbra <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status")

sppByIbra <- sppByIbra %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

sppByIbra <- sppByIbra %>%
  dplyr::filter(griis_status == "introduced")


# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)


sppByIbra1 <- setDT(sppByIbra)[, .(sppByIbra_introduced = .N),
                               keyby = c("IBRA", "YearRange", "griis_status")]

fwrite(sppByIbra1, "SumTable/sppByIbra_introduced.csv")

rm(sppByIbra, sppByIbra1)

# Introduced Species first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)
griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

griis$YearRange <- as.numeric(griis$YearRange)
df_final <- griis[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = griis)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_new_species",
                       "introduced_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced.csv")

rm(df_final, df_list, result_df, result_list, griis)

# Number of introduced species inside indigeneous PA
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInIndPa_introduced")

fwrite(sppInPa, "SumTable/sppInIndPa_introduced.csv")

rm(griis, pa, sppInPa)

# Introduced species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_sppInIndPa_new_species",
                       "introduced_sppInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppInInd.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Introduced species which are either inside (not outside) or outside (not inside) indigeneous PAs
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
Ind_pa <- unique(griis)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status")]

sppOnlyInIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppOnlyInIndPa <- sppOnlyInIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInIndPa)[3] <- "introduced_sppOnlyInIndPa"

sppOnlyOutIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOnlyOutIndPa <- sppOnlyOutIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutIndPa)[3] <- "introduced_sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "SumTable/introduced_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "SumTable/introduced_sppOnlyOutIndPa.csv")

rm(griis, Ind_pa, sppOnlyInIndPa, sppOnlyOutIndPa)

# Introduced species distributed only inside (not outside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_sppOnlyInIndPa_new_species",
                       "introduced_sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppOnlyInIndPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


# Introduced species distributed only outside (not inside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_sppOnlyOutIndPa_new_species",
                       "introduced_sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppOnlyOutIndPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


# Number of introduced species inside PA
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_introduced")

fwrite(sppInPa, "SumTable/sppInPa_introduced.csv")

rm(griis, pa, sppInPa)

# Introduced species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_sppInPa_new_species",
                       "introduced_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Introduced species which are either inside (not outside) or outside 
# (not inside) PAs
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
Ind_pa <- unique(griis)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "introduced_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "introduced_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/introduced_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/introduced_sppOnlyOutPa.csv")

rm(griis, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# Introduced species distributed only inside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_sppOnlyInPa_new_species",
                       "introduced_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppOnlyInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


# Introduced species distributed only outside (not inside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "introduced_sppOnlyOutPa_new_species",
                       "introduced_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppOnlyOutPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

##############################################################
# Number of invasive species by IBRA regions
sppByIbra <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status")

sppByIbra <- sppByIbra %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

sppByIbra <- sppByIbra %>%
  dplyr::filter(griis_status == "invasive")


# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)


sppByIbra1 <- setDT(sppByIbra)[, .(sppByIbra_invasive = .N),
                               keyby = c("IBRA", "YearRange", "griis_status")]

fwrite(sppByIbra1, "SumTable/sppByIbra_invasive.csv")

rm(sppByIbra, sppByIbra1)


# Invasive Species first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

griis$YearRange <- as.numeric(griis$YearRange)
df_final <- griis[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = griis)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_new_species",
                       "invasive_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive.csv")

rm(df_final, df_list, result_df, result_list, griis)

# Number of introduced species inside indigeneous PA
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInIndPa_invasive")

fwrite(sppInPa, "SumTable/sppInIndPa_invasive.csv")

rm(griis, pa, sppInPa)

# Invasive species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_sppInIndPa_new_species",
                       "invasive_sppInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppInInd.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Invasive species which are either inside (not outside) or outside (not inside) indigeneous PAs
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
Ind_pa <- unique(griis)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status")]

sppOnlyInIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppOnlyInIndPa <- sppOnlyInIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInIndPa)[3] <- "invasive_sppOnlyInIndPa"

sppOnlyOutIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOnlyOutIndPa <- sppOnlyOutIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutIndPa)[3] <- "invasive_sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "SumTable/invasive_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "SumTable/invasive_sppOnlyOutIndPa.csv")

rm(griis, Ind_pa, sppOnlyInIndPa, sppOnlyOutIndPa)

# Invasive species distributed only inside (not outside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_sppOnlyInIndPa_new_species",
                       "invasive_sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppOnlyInIndPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


# Invasive species distributed only outside (not inside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_sppOnlyOutIndPa_new_species",
                       "invasive_sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppOnlyOutIndPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


# Number of invasive species inside PA
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_invasive")

fwrite(sppInPa, "SumTable/sppInPa_invasive.csv")

rm(griis, pa, sppInPa)

# Invasive species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_sppInPa_new_species",
                       "invasive_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Invasive species which are either inside (not outside) or outside 
# (not inside) PAs
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
Ind_pa <- unique(griis)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "invasive_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "invasive_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/invasive_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/invasive_sppOnlyOutPa.csv")

rm(griis, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# Invasive species distributed only inside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_sppOnlyInPa_new_species",
                       "invasive_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppOnlyInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


# Invasive species distributed only outside (not inside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
pa <- unique(griis)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "invasive_sppOnlyOutPa_new_species",
                       "invasive_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppOnlyOutPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

##############################################################
# Number of wons species by IBRA regions
sppByIbra <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status")

sppByIbra <- sppByIbra %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)


sppByIbra1 <- setDT(sppByIbra)[, .(sppByIbra_wons = .N),
                               keyby = c("IBRA", "YearRange", "wons_status")]

fwrite(sppByIbra1, "SumTable/sppByIbra_wons.csv")

rm(sppByIbra, sppByIbra1)


# Wons Species first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

wons$YearRange <- as.numeric(wons$YearRange)
df_final <- wons[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = wons)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_new_species",
                       "WoNS_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS.csv")

rm(df_final, df_list, result_df, result_list, wons)

# Number of introduced species inside indigeneous PA
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

pa <- setDT(wons)[, .(count = .N), keyby = c("IBRA", "YearRange", 
                                             "indigenous_Status", "wons_status")]

sppInPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInIndPa_WoNS")

fwrite(sppInPa, "SumTable/sppInIndPa_WoNS.csv")

rm(wons, pa, sppInPa)

# WoNS species inside PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", 
                "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_sppInIndPa_new_species",
                       "WoNS_sppInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppInInd.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

# WoNS species which are either inside (not outside) or outside (not inside) indigeneous PAs
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", 
                "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
Ind_pa <- unique(wons)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", 
                                                   "indigenous_Status")]

sppOnlyInIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppOnlyInIndPa <- sppOnlyInIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInIndPa)[3] <- "WoNS_sppOnlyInIndPa"

sppOnlyOutIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOnlyOutIndPa <- sppOnlyOutIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutIndPa)[3] <- "WoNS_sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "SumTable/WoNS_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "SumTable/WoNS_sppOnlyOutIndPa.csv")

rm(wons, Ind_pa, sppOnlyInIndPa, sppOnlyOutIndPa)

# WoNS species distributed only inside (not outside) indigenous PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_sppOnlyInIndPa_new_species",
                       "WoNS_sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppOnlyInIndPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)


# WoNS species distributed only outside (not inside) indigenous PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_sppOnlyOutIndPa_new_species",
                       "WoNS_sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppOnlyOutIndPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)


# Number of WoNS species inside PA
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

pa <- setDT(wons)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status", "wons_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_WoNS")

fwrite(sppInPa, "SumTable/sppInPa_WoNS.csv")

rm(wons, pa, sppInPa)

# WoNS species inside PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", 
                "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_sppInPa_new_species",
                       "WoNS_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppInPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

# WoNS species which are either inside (not outside) or outside 
# (not inside) PAs
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", 
                "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
Ind_pa <- unique(wons)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "WoNS_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "WoNS_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/WoNS_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/WoNS_sppOnlyOutPa.csv")

rm(wons, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# WoNS species distributed only inside (not outside) PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_sppOnlyInPa_new_species",
                       "WoNS_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppOnlyInPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)


# WoNS species distributed only outside (not inside) indigenous PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "WoNS_sppOnlyOutPa_new_species",
                       "WoNS_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppOnlyOutPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

##############################################################
ala <- ala %>%
  dplyr::mutate(epbc_status = ifelse(conservation_status == "Non-threatened", 
                                     "Non-threatened", "epbc"))

# Number of epbc species by IBRA regions
sppByIbra <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status")

sppByIbra <- sppByIbra %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)


sppByIbra1 <- setDT(sppByIbra)[, .(sppByIbra_epbc = .N),
                               keyby = c("IBRA", "YearRange", "epbc_status")]

fwrite(sppByIbra1, "SumTable/sppByIbra_epbc.csv")

rm(sppByIbra, sppByIbra1)


# epbc Species first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

epbc$YearRange <- as.numeric(epbc$YearRange)
df_final <- epbc[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = epbc)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_new_species",
                       "epbc_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc.csv")

rm(df_final, df_list, result_df, result_list, epbc)

# Number of introduced species inside indigeneous PA
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("IBRA", "YearRange", 
                                             "indigenous_Status", "epbc_status")]

sppInPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInIndPa_epbc")

fwrite(sppInPa, "SumTable/sppInIndPa_epbc.csv")

rm(epbc, pa, sppInPa)

# epbc species inside PA first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", 
                "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_sppInIndPa_new_species",
                       "epbc_sppInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppInInd.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species which are either inside (not outside) or outside (not inside) indigeneous PAs
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", 
                "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
Ind_pa <- unique(epbc)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", 
                                                   "indigenous_Status")]

sppOnlyInIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppOnlyInIndPa <- sppOnlyInIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInIndPa)[3] <- "epbc_sppOnlyInIndPa"

sppOnlyOutIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOnlyOutIndPa <- sppOnlyOutIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutIndPa)[3] <- "epbc_sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "SumTable/epbc_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "SumTable/epbc_sppOnlyOutIndPa.csv")

rm(epbc, Ind_pa, sppOnlyInIndPa, sppOnlyOutIndPa)

# epbc species distributed only inside (not outside) indigenous PA first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_sppOnlyInIndPa_new_species",
                       "epbc_sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppOnlyInIndPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)


# epbc species distributed only outside (not inside) indigenous PA first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_sppOnlyOutIndPa_new_species",
                       "epbc_sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppOnlyOutIndPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)


# Number of epbc species inside PA
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status", "epbc_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IBRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_epbc")

fwrite(sppInPa, "SumTable/sppInPa_epbc.csv")

rm(epbc, pa, sppInPa)

# epbc species inside PA first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_sppInPa_new_species",
                       "epbc_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppInPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species which are either inside (not outside) or outside 
# (not inside) PAs
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
Ind_pa <- unique(epbc)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "epbc_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "epbc_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/epbc_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/epbc_sppOnlyOutPa.csv")

rm(epbc, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# epbc species distributed only inside (not outside) PA first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_sppOnlyInPa_new_species",
                       "epbc_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppOnlyInPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)


# epbc species distributed only outside (not inside) indigenous PA first/last seen count
epbc <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IBRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IBRA == a$IBRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IBRA == a$IBRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IBRA == a$IBRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  number_of_species <- length(unique(current_year_species))
  return(c(
    new_species = new_species, 
    last_seen_species = last_seen_species))
}, x = pa)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(IBRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IBRA", "YearRange", "epbc_sppOnlyOutPa_new_species",
                       "epbc_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppOnlyOutPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)


############################################
# Merging datafiles
list <- list.files(path = "SumTable/Mammalia", pattern = ".csv", full.names = TRUE)

csv <- lapply(list, fread)

df1 <- fread(list[[1]])
df2 <- fread(list[[2]])
df3 <- fread(list[[3]])
df4 <- fread(list[[4]])
df5 <- fread(list[[5]])
df6 <- fread(list[[6]])
df7 <- fread(list[[7]])
df8 <- fread(list[[8]])
df9 <- fread(list[[9]])
df10 <- fread(list[[10]])
df11 <- fread(list[[11]])
df12 <- fread(list[[12]])
df13 <- fread(list[[13]])
df14 <- fread(list[[14]])
df15 <- fread(list[[15]])
df16 <- fread(list[[16]])
df17 <- fread(list[[17]])
df18 <- fread(list[[18]])
df19 <- fread(list[[19]])
df20 <- fread(list[[20]])
df21 <- fread(list[[21]])
df22 <- fread(list[[22]])
df23 <- fread(list[[23]])
df24 <- fread(list[[24]])
df25 <- fread(list[[25]])
df26 <- fread(list[[26]])
df27 <- fread(list[[27]])
df28 <- fread(list[[28]])
df29 <- fread(list[[29]])
df30 <- fread(list[[30]])
df31 <- fread(list[[31]])
df32 <- fread(list[[32]])
df33 <- fread(list[[33]])
df34 <- fread(list[[34]])
df35 <- fread(list[[35]])
df36 <- fread(list[[36]])
df37 <- fread(list[[37]])
df38 <- fread(list[[38]])
df39 <- fread(list[[39]])
df40 <- fread(list[[40]])
df41 <- fread(list[[41]])
df42 <- fread(list[[42]])
df43 <- fread(list[[43]])
df44 <- fread(list[[44]])
df45 <- fread(list[[45]])
df46 <- fread(list[[46]])
df47 <- fread(list[[47]])
df48 <- fread(list[[48]])
df49 <- fread(list[[49]])
df50 <- fread(list[[50]])
df51 <- fread(list[[51]])
df51 <- df51 %>%
  dplyr::select(IBRA, YearRange, sppByIbra_epbc)
df52 <- fread(list[[52]])
df52 <- df52 %>%
  dplyr::select(IBRA, YearRange, sppByIbra_introduced)
df53 <- fread(list[[53]])
df53 <- df53 %>%
  dplyr::select(IBRA, YearRange, sppByIbra_invasive)
df54 <- fread(list[[54]])
df54 <- df54 %>%
  dplyr::select(IBRA, YearRange, sppByIbra_wons)
df55 <- fread(list[[55]])
df56 <- fread(list[[56]])
df57 <- fread(list[[57]])
df58 <- fread(list[[58]])
df59 <- fread(list[[59]])
df60 <- fread(list[[60]])
df61 <- fread(list[[61]])
df62 <- fread(list[[62]])
df63 <- fread(list[[63]])
df64 <- fread(list[[64]])
df65 <- fread(list[[65]])
df65 <- df65 %>%
  dplyr::select(IBRA, YearRange, count)
colnames(df65)[3] <- "sppOnlyInIndPa"
df66 <- fread(list[[66]])
df67 <- fread(list[[67]])
df67 <- df67 %>%
  dplyr::select(IBRA, YearRange, count)
colnames(df67)[3] <- "sppOnlyOutIndPa"
df68 <- fread(list[[68]])
df69 <- fread(list[[69]])
df70 <- fread(list[[70]])
df71 <- fread(list[[71]])
df72 <- fread(list[[72]])
df73 <- fread(list[[73]])

df <- df14 %>%
  dplyr::left_join(df1, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df2, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df3, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df4, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df5, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df6, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df7, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df8, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df9, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df10, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df11, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df12, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df13, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df15, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df16, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df17, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df18, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df19, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df20, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df21, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df22, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df23, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df24, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df25, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df26, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df27, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df28, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df29, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df30, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df31, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df32, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df33, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df34, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df35, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df36, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df37, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df38, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df39, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df40, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df41, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df42, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df43, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df44, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df45, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df46, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df47, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df48, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df49, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df50, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df51, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df52, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df53, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df54, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df55, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df56, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df57, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df58, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df59, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df60, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df61, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df62, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df63, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df64, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df65, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df66, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df67, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df68, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df68, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df69, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df70, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df71, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df72, by = c("IBRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df73, by = c("IBRA", "YearRange"))

View(df)

write.csv(df, "SumTable/FullMerged_Mammalia.csv")

