options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Merging data
input_folder <- "cache/Merged/Marine/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

colnames(ala)[18] <- c("CM_Zone")

# Subsetting data
ala <- ala %>%
  dplyr::select(species_guid, year, basisOfRecord,
                IMCRA, CAPAD_Status, wons_status, griis_status,
                conservation_status, CM_Zone)

# Renaming some columns
colnames(ala) <- c("species_guid", "year", "basisOfRecord",
                   "IMCRA", "capad_status", "wons_status", "griis_status",
                   "conservation_status", "ConservManageZone")

# Removing blank cells
ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]
ala <- ala[!(is.na(ala$IMCRA) | ala$IMCRA == ""),]


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
occByIMCRA <- ala %>%
  dplyr::select("IMCRA", "YearRange")
occByIMCRA <- setDT(occByIMCRA)[, .(occByIMCRA = .N), keyby = c("IMCRA", "YearRange")]

fwrite(occByIMCRA, "SumTable/occByIMCRA.csv")

rm(occByIMCRA)

# Species count
sppByIMCRA <- ala %>%
  dplyr::select("species_guid", "IMCRA", "YearRange")

# Removing duplicates
setkey(sppByIMCRA,NULL)
sppByIMCRA <- unique(sppByIMCRA)

sppByIMCRA <- setDT(sppByIMCRA)[, .(sppByIMCRA = .N), keyby = c("IMCRA", "YearRange")]
fwrite(sppByIMCRA, "SumTable/sppByIMCRA.csv")
rm(sppByIMCRA)

# Number of citizen science records
cs <- ala %>%
  dplyr::select(IMCRA, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HumanObservation", "CitizenScience", "OtherSource"))
cs <- cs %>%
  dplyr::filter(basisOfRecord == "CitizenScience")


cs <- setDT(cs)[, .(CS_records = .N), keyby = c("IMCRA", "YearRange", "basisOfRecord")]
cs <- cs %>%
  dplyr::select(IMCRA, YearRange, CS_records)

fwrite(cs, "SumTable/NumberOfCitizenScienceRecords.csv")
rm(cs)

# Species first/last seen count
ala_simple <- setDT(ala)[,.(.N), c("IMCRA", "YearRange", "species_guid")]

ala_simple$YearRange <- as.numeric(ala_simple$YearRange)
df_final <- ala_simple[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  cumulative_species <-length(unique(
    x[IMCRA == a$IMCRA & YearRange <= a$YearRange, "species_guid"]$species_guid
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved.csv")

rm(ala_simple, df_final, df_list, result_df, result_list, data, input_folder)

# Species inside PA count
pa <- ala %>%
  dplyr::select(IMCRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(sppInPa = .N), keyby = c("IMCRA", "YearRange", "capad_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IMCRA, YearRange, sppInPa)

fwrite(sppInPa, "SumTable/sppInPa.csv")

rm(pa, sppInPa)

# Species inside PA first/last seen count
pa <- ala %>%
  dplyr::select(IMCRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "sppInPa_new_species",
                       "sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species which are either inside (not outside) or outside (not inside) PAs
pa <- ala %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status")]

sppOnlyInPa <- pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "sppOnlyInPa"

sppOnlyOutPa <- pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "sppOnlyOutPa"


fwrite(sppOnlyInPa, "SumTable/sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/sppOnlyOutPa.csv")

rm(pa, sppOnlyInPa, sppOnlyOutPa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(IMCRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "sppOnlyInPa_new_species",
                       "sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppOnlyInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(IMCRA, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "sppOnlyOutPa_new_species",
                       "sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_sppOnlyOutPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

#############################################################################
# Number of introduced species by IMCRA regions
sppByIMCRA <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status")

sppByIMCRA <- sppByIMCRA %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

sppByIMCRA <- sppByIMCRA %>%
  dplyr::filter(griis_status == "introduced")


# Removing duplicates
setkey(sppByIMCRA,NULL)
sppByIMCRA <- unique(sppByIMCRA)


sppByIMCRA1 <- setDT(sppByIMCRA)[, .(sppByIMCRA_introduced = .N),
                               keyby = c("IMCRA", "YearRange", "griis_status")]

fwrite(sppByIMCRA1, "SumTable/sppByIMCRA_introduced.csv")

rm(sppByIMCRA, sppByIMCRA1)

# Introduced Species first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status")

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
df_final <- griis[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "introduced_new_species",
                       "introduced_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced.csv")

rm(df_final, df_list, result_df, result_list, griis)

# Number of introduced species inside PA
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IMCRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_introduced")

fwrite(sppInPa, "SumTable/sppInPa_introduced.csv")

rm(griis, pa, sppInPa)

# Introduced species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
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
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "introduced_sppInPa_new_species",
                       "introduced_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Introduced species which are either inside (not outside) or outside 
# (not inside) PAs
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
Ind_pa <- unique(griis)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "introduced_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "introduced_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/introduced_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/introduced_sppOnlyOutPa.csv")

rm(griis, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# Introduced species distributed only inside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", "capad_status")

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
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "introduced_sppOnlyInPa_new_species",
                       "introduced_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppOnlyInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Introduced species distributed only outside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", "capad_status")

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
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "introduced_sppOnlyOutPa_new_species",
                       "introduced_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_introduced_sppOnlyOutPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

##############################################################
# Number of invasive species by IMCRA regions
sppByIMCRA <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status")

sppByIMCRA <- sppByIMCRA %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

sppByIMCRA <- sppByIMCRA %>%
  dplyr::filter(griis_status == "invasive")


# Removing duplicates
setkey(sppByIMCRA,NULL)
sppByIMCRA <- unique(sppByIMCRA)


sppByIMCRA1 <- setDT(sppByIMCRA)[, .(sppByIMCRA_invasive = .N),
                               keyby = c("IMCRA", "YearRange", "griis_status")]

fwrite(sppByIMCRA1, "SumTable/sppByIMCRA_invasive.csv")

rm(sppByIMCRA, sppByIMCRA1)


# Invasive Species first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

griis$YearRange <- as.numeric(griis$YearRange)
df_final <- griis[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "invasive_new_species",
                       "invasive_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive.csv")

rm(df_final, df_list, result_df, result_list, griis)

# Number of invasive species inside PA
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IMCRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_invasive")

fwrite(sppInPa, "SumTable/sppInPa_invasive.csv")

rm(griis, pa, sppInPa)

# Invasive species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
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
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "invasive_sppInPa_new_species",
                       "invasive_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Invasive species which are either inside (not outside) or outside 
# (not inside) PAs
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

# Removing duplicates
setkey(griis,NULL)
Ind_pa <- unique(griis)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "invasive_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "invasive_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/invasive_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/invasive_sppOnlyOutPa.csv")

rm(griis, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# Invasive species distributed only inside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", "capad_status")

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
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "invasive_sppOnlyInPa_new_species",
                       "invasive_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppOnlyInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Invasive species distributed only outside (not inside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", "capad_status")

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
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "invasive_sppOnlyOutPa_new_species",
                       "invasive_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_invasive_sppOnlyOutPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)


##############################################################
# Number of wons species by IMCRA regions
sppByIMCRA <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status")

sppByIMCRA <- sppByIMCRA %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(sppByIMCRA,NULL)
sppByIMCRA <- unique(sppByIMCRA)


sppByIMCRA1 <- setDT(sppByIMCRA)[, .(sppByIMCRA_wons = .N),
                               keyby = c("IMCRA", "YearRange", "wons_status")]

fwrite(sppByIMCRA1, "SumTable/sppByIMCRA_wons.csv")

rm(sppByIMCRA, sppByIMCRA1)


# Wons Species first/last seen count
wons <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

wons$YearRange <- as.numeric(wons$YearRange)
df_final <- wons[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "WoNS_new_species",
                       "WoNS_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS.csv")

rm(df_final, df_list, result_df, result_list, wons)

# Number of WoNS species inside PA
wons <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
wons <- unique(wons)

pa <- setDT(wons)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status", "wons_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IMCRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_WoNS")

fwrite(sppInPa, "SumTable/sppInPa_WoNS.csv")

rm(wons, pa, sppInPa)

# WoNS species inside PA first/last seen count
wons <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status", 
                "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "WoNS_sppInPa_new_species",
                       "WoNS_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppInPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

# WoNS species which are either inside (not outside) or outside 
# (not inside) PAs
wons <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status", 
                "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
Ind_pa <- unique(wons)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "WoNS_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "WoNS_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/WoNS_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/WoNS_sppOnlyOutPa.csv")

rm(wons, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# WoNS species distributed only inside (not outside) PA first/last seen count
wons <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "WoNS_sppOnlyInPa_new_species",
                       "WoNS_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppOnlyInPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

# WoNS species distributed only outside (not inside) PA first/last seen count
wons <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

# Removing duplicates
setkey(wons,NULL)
pa <- unique(wons)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "WoNS_sppOnlyOutPa_new_species",
                       "WoNS_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_WoNS_sppOnlyOutPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)


##############################################################
ala <- ala %>%
  dplyr::mutate(epbc_status = ifelse(conservation_status == "Non-threatened", 
                                     "Non-threatened", "epbc"))

# Number of epbc species by IMCRA regions
sppByIMCRA <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status")

sppByIMCRA <- sppByIMCRA %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(sppByIMCRA,NULL)
sppByIMCRA <- unique(sppByIMCRA)


sppByIMCRA1 <- setDT(sppByIMCRA)[, .(sppByIMCRA_epbc = .N),
                               keyby = c("IMCRA", "YearRange", "epbc_status")]

fwrite(sppByIMCRA1, "SumTable/sppByIMCRA_epbc.csv")

rm(sppByIMCRA, sppByIMCRA1)


# epbc Species first/last seen count
epbc <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

epbc$YearRange <- as.numeric(epbc$YearRange)
df_final <- epbc[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "epbc_new_species",
                       "epbc_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc.csv")

rm(df_final, df_list, result_df, result_list, epbc)

# Number of epbc species inside PA
epbc <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status", "epbc_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(IMCRA, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_epbc")

fwrite(sppInPa, "SumTable/sppInPa_epbc.csv")

rm(epbc, pa, sppInPa)

# epbc species inside PA first/last seen count
epbc <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "epbc_sppInPa_new_species",
                       "epbc_sppInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppInPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species which are either inside (not outside) or outside 
# (not inside) PAs
epbc <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
Ind_pa <- unique(epbc)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IMCRA", "YearRange", "capad_status")]

sppOnlyInPa <- Ind_pa %>%
  dplyr::filter(capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "epbc_sppOnlyInPa"

sppOnlyOutPa <- Ind_pa %>%
  dplyr::filter(capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IMCRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "epbc_sppOnlyOutPa"

fwrite(sppOnlyInPa, "SumTable/epbc_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "SumTable/epbc_sppOnlyOutPa.csv")

rm(epbc, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# epbc species distributed only inside (not outside) PA first/last seen count
epbc <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "epbc_sppOnlyInPa_new_species",
                       "epbc_sppOnlyInPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppOnlyInPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species distributed only outside (not inside) PA first/last seen count
epbc <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
pa <- pa %>%
  dplyr::filter(capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("IMCRA", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[IMCRA == a$IMCRA & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[IMCRA == a$IMCRA & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[IMCRA == a$IMCRA & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(IMCRA, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("IMCRA", "YearRange", "epbc_sppOnlyOutPa_new_species",
                       "epbc_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "SumTable/SpeciesFirst&LastObserved_epbc_sppOnlyOutPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)


############################################
# Merging datafiles
list <- list.files(path = "SumTable/Marine", pattern = ".csv", full.names = TRUE)

csv <- lapply(list, fread)
View(csv)

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
df30 <- df30 %>%
  dplyr::select(IMCRA, YearRange, sppByIMCRA_epbc)
df31 <- fread(list[[31]])
df31 <- df31 %>%
  dplyr::select(IMCRA, YearRange, sppByIMCRA_introduced)
df32 <- fread(list[[32]])
df32 <- df32 %>%
  dplyr::select(IMCRA, YearRange, sppByIMCRA_invasive)
df33 <- fread(list[[33]])
df33 <- df33 %>%
  dplyr::select(IMCRA, YearRange, sppByIMCRA_wons)
df34 <- fread(list[[34]])
df35 <- fread(list[[35]])
df36 <- fread(list[[36]])
df37 <- fread(list[[37]])
df38 <- fread(list[[38]])
df39 <- fread(list[[39]])
df40 <- fread(list[[40]])
df41 <- fread(list[[41]])
df42 <- fread(list[[42]])


df <- df8 %>%
  dplyr::left_join(df1, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df2, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df3, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df4, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df5, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df6, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df7, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df9, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df10, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df11, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df12, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df13, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df14, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df15, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df16, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df17, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df18, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df19, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df20, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df21, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df22, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df23, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df24, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df25, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df26, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df27, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df28, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df29, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df30, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df31, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df32, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df33, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df34, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df35, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df36, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df37, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df38, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df39, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df40, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df41, by = c("IMCRA", "YearRange"))
df <- df %>%
  dplyr::left_join(df42, by = c("IMCRA", "YearRange"))


write.csv(df, "SumTable/FullMerged_marine.csv")

