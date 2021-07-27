# For all speies 
# Occurrence records count
occByIbra <- ala %>%
  dplyr::select("IBRA", "YearRange")
occByIbra <- setDT(occByIbra)[, .(occByIbra = .N), keyby = c("IBRA", "YearRange")]

fwrite(occByIbra, "cache/sumTable/ibra/occByIbra.csv")

rm(occByIbra)

# Species count
sppByIbra <- ala %>%
  dplyr::select("species_guid", "IBRA", "YearRange")

# Removing duplicates
setkey(sppByIbra,NULL)
sppByIbra <- unique(sppByIbra)

sppByIbra <- setDT(sppByIbra)[, .(sppByIbra = .N), keyby = c("IBRA", "YearRange")]
fwrite(sppByIbra, "cache/sumTable/ibra/sppByIbra.csv")
rm(sppByIbra)

# Number of citizen science records
cs <- ala %>%
  dplyr::select(IBRA, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HUMAN_OBSERVATION", "CitizenScience", "OtherSource"))
cs <- cs %>%
  dplyr::filter(basisOfRecord == "CitizenScience")


cs <- setDT(cs)[, .(CS_records = .N), keyby = c("IBRA", "YearRange", "basisOfRecord")]
cs <- cs %>%
  dplyr::select(IBRA, YearRange, CS_records)

fwrite(cs, "cache/sumTable/ibra/NumberOfCitizenScienceRecords.csv")
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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved.csv")

rm(ala_simple, df_final, df_list, result_df, result_list, data, input_folder)

# Number of species inside indigenous PA
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

fwrite(sppInPa, "cache/sumTable/ibra/sppInIndPa.csv")
fwrite(sppOutPa, "cache/sumTable/ibra/sppOutIndPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_sppInIndPa.csv")

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

fwrite(sppInPa, "cache/sumTable/ibra/sppInPa.csv")

rm(pa, sppInPa)

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_sppInPa.csv")

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


fwrite(sppOnlyInPa, "cache/sumTable/ibra/sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/ibra/sppOnlyOutPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_sppOnlyInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species distributed only outside (not inside) PA first/last seen count
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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_sppOnlyOutPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species which are either inside (not outside) or outside (not inside) indigeneous PAs
Ind_pa <- ala %>%
  dplyr::select(species_guid, IBRA, YearRange, indigenous_Status)

# Removing duplicates
setkey(Ind_pa,NULL)
Ind_pa <- unique(Ind_pa)

Ind_pa <- setDT(Ind_pa)[, .(count = .N), keyby = c("IBRA", "YearRange", "indigenous_Status")]

sppOnlyInIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppOnlyInIndPa <- sppOnlyInIndPa %>% 
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInIndPa)[3] <- "sppOnlyInIndPa"

sppOnlyOutIndPa <- Ind_pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOnlyOutIndPa <- sppOnlyOutIndPa %>% 
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutIndPa)[3] <- "sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "cache/sumTable/ibra/sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "cache/sumTable/ibra/sppOnlyOutIndPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_sppOnlyInIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species distributed only outside (not inside) PA first/last seen count
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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_sppOnlyOutIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa)