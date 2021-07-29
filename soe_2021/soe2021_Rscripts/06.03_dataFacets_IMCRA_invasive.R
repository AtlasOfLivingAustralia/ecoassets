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
sppByIMCRA1 <- sppByIMCRA1 %>% 
  dplyr::select(IMCRA, YearRange, sppByIMCRA_invasive)

fwrite(sppByIMCRA1, "cache/sumTable/imcra/sppByIMCRA_invasive.csv")

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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_invasive.csv")

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

fwrite(sppInPa, "cache/sumTable/imcra/sppInPa_invasive.csv")

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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_invasive_sppInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# invasive species which are either inside (not outside) or outside (not inside) PAs
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")
griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

pa <- griis %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "IMCRA", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IMCRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IMCRA", "YearRange"))

sppOnlyInPa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")
sppOnlyInPa <- setDT(sppOnlyInPa)[, .(count = .N), keyby = c("IMCRA", "YearRange")]
colnames(sppOnlyInPa)[3] <- "invasive_sppOnlyInPa"

sppOnlyOutPa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")
sppOnlyOutPa <- setDT(sppOnlyOutPa)[, .(count = .N), keyby = c("IMCRA", "YearRange")]
colnames(sppOnlyOutPa)[3] <- "invasive_sppOnlyOutPa"

fwrite(sppOnlyInPa, "cache/sumTable/imcra/invasive_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/imcra/invasive_sppOnlyOutPa.csv")

rm(griis, sppOnlyInPa, sppOnlyOutPa, pa, pa1, pa2)

# invasive species distributed only inside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

pa <- griis %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "IMCRA", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IMCRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IMCRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")


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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_invasive_sppOnlyInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# invasive species distributed only outside (not inside) PA first/last seen count
griis <- ala %>%
  dplyr::select("IMCRA", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "INVASIVE", "invasive", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "invasive")

pa <- griis %>%
  dplyr::select(species_guid, IMCRA, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "IMCRA", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IMCRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IMCRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")


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

fwrite(df_final, "cache/sumTable/imcra/SpeciesFirst&LastObserved_invasive_sppOnlyOutPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa, pa1, pa2)
