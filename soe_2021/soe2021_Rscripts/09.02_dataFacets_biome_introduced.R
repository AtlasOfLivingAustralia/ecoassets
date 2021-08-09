# Number of introduced species by biome regions
sppBybiome <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status")

sppBybiome <- sppBybiome %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

sppBybiome <- sppBybiome %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(sppBybiome,NULL)
sppBybiome <- unique(sppBybiome)


sppBybiome1 <- setDT(sppBybiome)[, .(sppBybiome_introduced = .N),
                               keyby = c("biome", "YearRange", "griis_status")]
sppBybiome1 <- sppBybiome1 %>% 
  dplyr::select(biome, YearRange, sppBybiome_introduced)

fwrite(sppBybiome1, "cache/sumTable/biome/sppBybiome_introduced.csv")

rm(sppBybiome, sppBybiome1)

# Introduced Species first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status")

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
df_final <- griis[, .(.N), keyby = c("biome", "YearRange")]

df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_new_species",
                       "introduced_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced.csv")

rm(df_final, df_list, result_df, result_list, griis)

# Number of introduced species inside indigenous PA
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("biome", "YearRange", "indigenous_Status", "griis_status")]

sppInIndPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppInIndPa <- sppInIndPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInIndPa)[3] <- c("sppInIndPa_introduced")

fwrite(sppInIndPa, "cache/sumTable/biome/sppInIndPa_introduced.csv")

rm(griis, pa, sppInIndPa)

# Number of introduced species inside indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", "indigenous_Status")

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
df_final <- pa[, .(.N), keyby = c("biome", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_sppInIndPa_new_species",
                       "introduced_sppInIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced_sppInInd.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Introduced species which are either inside (not outside) or outside (not inside) PAs
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
                "capad_status")
griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

pa <- griis %>%
  dplyr::select(species_guid, biome, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

sppOnlyInPa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")
sppOnlyInPa <- setDT(sppOnlyInPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyInPa)[3] <- "introduced_sppOnlyInPa"

sppOnlyOutPa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")
sppOnlyOutPa <- setDT(sppOnlyOutPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyOutPa)[3] <- "introduced_sppOnlyOutPa"

fwrite(sppOnlyInPa, "cache/sumTable/biome/introduced_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/biome/introduced_sppOnlyOutPa.csv")

rm(griis, sppOnlyInPa, sppOnlyOutPa, pa, pa1, pa2)

# Introduced species distributed only inside (not outside) PA first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

pa <- griis %>%
  dplyr::select(species_guid, biome, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")


pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("biome", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_sppOnlyInPa_new_species",
                       "introduced_sppOnlyInPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced_sppOnlyInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# Introduced species distributed only outside (not inside) PA first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
                "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

pa <- griis %>%
  dplyr::select(species_guid, biome, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")


pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("biome", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_sppOnlyOutPa_new_species",
                       "introduced_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced_sppOnlyOutPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa, pa1, pa2)


# Number of introduced species inside PA
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", "capad_status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

# Removing duplicates
setkey(griis,NULL)
griis <- unique(griis)

pa <- setDT(griis)[, .(count = .N), keyby = c("biome", "YearRange", "capad_status", "griis_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_introduced")

fwrite(sppInPa, "cache/sumTable/biome/sppInPa_introduced.csv")

rm(griis, pa, sppInPa)

# Introduced species inside PA first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
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
df_final <- pa[, .(.N), keyby = c("biome", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_sppInPa_new_species",
                       "introduced_sppInPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced_sppInPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa)

# Introduced species which are either inside (not outside) or outside (not inside) indigenous PAs
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
                "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

pa <- griis %>%
  dplyr::select(species_guid, biome, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

sppOnlyInIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")
sppOnlyInIndPa <- setDT(sppOnlyInIndPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyInIndPa)[3] <- "introduced_sppOnlyInIndPa"

sppOnlyOutIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")
sppOnlyOutIndPa <- setDT(sppOnlyOutIndPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyOutIndPa)[3] <- "introduced_ssppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "cache/sumTable/biome/introduced_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "cache/sumTable/biome/introduced_sppOnlyOutIndPa.csv")

rm(griis, sppOnlyInIndPa, sppOnlyOutIndPa, pa, pa1, pa2)

# Introduced species distributed only inside (not outside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
                "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

pa <- griis %>%
  dplyr::select(species_guid, biome, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("biome", "YearRange")]

df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_sppOnlyInIndPa_new_species",
                       "introduced_sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced_sppOnlyInIndPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# Introduced species distributed only outside (not inside) indigenous PA first/last seen count
griis <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "griis_status", 
                "indigenous_Status")

griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(griis_status == "NULL", "introduced", "other"))

griis <- griis %>%
  dplyr::filter(griis_status == "introduced")

pa <- griis %>%
  dplyr::select(species_guid, biome, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")


pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("biome", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[biome == a$biome & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[biome == a$biome & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[biome == a$biome & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "introduced_sppOnlyOutIndPa_new_species",
                       "introduced_sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_introduced_sppOnlyOutIndPa.csv")

rm(griis, df_final, df_list, result_df, result_list, pa, pa1, pa2)