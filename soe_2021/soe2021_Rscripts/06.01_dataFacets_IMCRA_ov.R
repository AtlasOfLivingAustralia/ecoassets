# For all species
# Occurrence records count
occByIMCRA <- ala %>%
  dplyr::select("IMCRA", "YearRange")
occByIMCRA <- setDT(occByIMCRA)[, .(occByIMCRA = .N), keyby = c("IMCRA", "YearRange")]

fwrite(occByIMCRA, "cache/sumTable/imcra/occByIMCRA.csv")

rm(occByIMCRA)

# Species count
sppByIMCRA <- ala %>%
  dplyr::select("species_guid", "IMCRA", "YearRange")

# Removing duplicates
setkey(sppByIMCRA,NULL)
sppByIMCRA <- unique(sppByIMCRA)

sppByIMCRA <- setDT(sppByIMCRA)[, .(sppByIMCRA = .N), keyby = c("IMCRA", "YearRange")]
fwrite(sppByIMCRA, "cache/sumTable/imcra/sppByIMCRA.csv")
rm(sppByIMCRA)

# Number of citizen science records
cs <- ala %>%
  dplyr::select(IMCRA, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HUMAN_OBSERVATION", "CitizenScience", "OtherSource"))
cs <- cs %>%
  dplyr::filter(basisOfRecord == "CitizenScience")


cs <- setDT(cs)[, .(CS_records = .N), keyby = c("IMCRA", "YearRange", "basisOfRecord")]
cs <- cs %>%
  dplyr::select(IMCRA, YearRange, CS_records)

fwrite(cs, "cache/sumTable/imcra/NumberOfCitizenScienceRecords.csv")
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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved.csv")

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

fwrite(sppInPa, "cache/sumTable/imcra/sppInPa.csv")

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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_sppInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species which are either inside (not outside) or outside (not inside) PAs
pa <- ala %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "IMCRA", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IMCRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IMCRA", "YearRange"))

sppOnlyInPa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")
sppOnlyInPa <- setDT(sppOnlyInPa)[, .(count = .N), keyby = c("IMCRA", "YearRange")]
colnames(sppOnlyInPa)[3] <- "sppOnlyInPa"

sppOnlyOutPa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")
sppOnlyOutPa <- setDT(sppOnlyOutPa)[, .(count = .N), keyby = c("IMCRA", "YearRange")]
colnames(sppOnlyOutPa)[3] <- "sppOnlyOutPa"

fwrite(sppOnlyInPa, "cache/sumTable/imcra/sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/imcra/sppOnlyOutPa.csv")

rm(pa, pa1, pa2, sppOnlyInPa, sppOnlyOutPa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "IMCRA", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IMCRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IMCRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")

pa <- pa %>%
  dplyr::select(species_guid, IMCRA, YearRange)

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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_sppOnlyInPa.csv")

rm(df_final, df_list, result_df, result_list, pa, pa1, pa2)

# Species distributed only outside (not inside) PA first/last seen count
pa <- ala %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "IMCRA", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IMCRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IMCRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")

pa <- pa %>%
  dplyr::select(species_guid, IMCRA, YearRange)

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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_sppOnlyOutPa.csv")

rm(df_final, df_list, result_df, result_list, pa, pa1, pa2)
