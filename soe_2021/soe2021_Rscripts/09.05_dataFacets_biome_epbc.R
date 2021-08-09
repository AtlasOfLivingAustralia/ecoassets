ala <- ala %>%
  dplyr::mutate(epbc_status = ifelse(conservation_status == "Non-threatened", 
                                     "Non-threatened", "epbc"))

# Number of epbc species by biome regions
sppBybiome <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status")

sppBybiome <- sppBybiome %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(sppBybiome,NULL)
sppBybiome <- unique(sppBybiome)


sppBybiome1 <- setDT(sppBybiome)[, .(sppBybiome_epbc = .N),
                               keyby = c("biome", "YearRange", "epbc_status")]
sppBybiome1 <- sppBybiome1 %>% 
  dplyr::select(biome, YearRange, sppBybiome_epbc)

fwrite(sppBybiome1, "cache/sumTable/biome/sppBybiome_epbc.csv")

rm(sppBybiome, sppBybiome1)


# epbc Species first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

epbc$YearRange <- as.numeric(epbc$YearRange)
df_final <- epbc[, .(.N), keyby = c("biome", "YearRange")]


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
}, x = epbc)

result_df <- as.data.frame(do.call(rbind, result_list))
df_final <- cbind(df_final, result_df)

# convert date range back to a factor
df_final$YearRange <- factor(
  df_final$YearRange,
  levels = seq_len(9),
  labels = levels(ala$YearRange))

df_final <- df_final %>%
  dplyr::select(biome, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("biome", "YearRange", "epbc_new_species",
                       "epbc_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc.csv")

rm(df_final, df_list, result_df, result_list, epbc)

# Number of introduced species inside indigenous PA
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("biome", "YearRange", 
                                             "indigenous_Status", "epbc_status")]

sppInIndPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

sppInIndPa <- sppInIndPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInIndPa)[3] <- c("sppInIndPa_epbc")

fwrite(sppInIndPa, "cache/sumTable/biome/sppInIndPa_epbc.csv")

rm(epbc, pa, sppInIndPa)

# epbc species inside indigenous PA first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
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
colnames(df_final)<- c("biome", "YearRange", "epbc_sppInIndPa_new_species",
                       "epbc_sppInIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc_sppInIndPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species which are either inside (not outside) or outside (not inside) indigenous PAs
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "indigenous_Status")
epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")
pa <- epbc %>%
  dplyr::select(species_guid, biome, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

sppOnlyInIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")
sppOnlyInIndPa <- setDT(sppOnlyInIndPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyInIndPa)[3] <- "epbc_sppOnlyInIndPa"

sppOnlyOutIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")
sppOnlyOutIndPa <- setDT(sppOnlyOutIndPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyOutIndPa)[3] <- "epbc_sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "cache/sumTable/biome/epbc_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "cache/sumTable/biome/epbc_sppOnlyOutIndPa.csv")

rm(epbc, sppOnlyInIndPa, sppOnlyOutIndPa, pa, pa1, pa2)

# epbc species distributed only inside (not outside) indigenous PA first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "indigenous_Status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

pa <- epbc %>%
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
colnames(df_final)<- c("biome", "YearRange", "epbc_sppOnlyInIndPa_new_species",
                       "epbc_sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc_sppOnlyInIndPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# epbc species distributed only outside (not inside) indigenous PA first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "indigenous_Status")
epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")
pa <- epbc %>%
  dplyr::select(species_guid, biome, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
pa <- setDT(pa)
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
colnames(df_final)<- c("biome", "YearRange", "epbc_sppOnlyOutIndPa_new_species",
                       "epbc_sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc_sppOnlyOutIndPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa, pa1, pa2)


# Number of epbc species inside PA
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
epbc <- unique(epbc)

pa <- setDT(epbc)[, .(count = .N), keyby = c("biome", "YearRange", "capad_status", "epbc_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(biome, YearRange, count)

colnames(sppInPa)[3] <- c("sppInPa_epbc")

fwrite(sppInPa, "cache/sumTable/biome/sppInPa_epbc.csv")

rm(epbc, pa, sppInPa)

# epbc species inside PA first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "capad_status")
epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

# Removing duplicates
setkey(epbc,NULL)
pa <- unique(epbc)
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
colnames(df_final)<- c("biome", "YearRange", "epbc_sppInPa_new_species",
                       "epbc_sppInPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc_sppInPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species which are either inside (not outside) or outside (not inside) PAs
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

pa <- epbc %>%
  dplyr::select(species_guid, biome, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

sppOnlyInPa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")
sppOnlyInPa <- setDT(sppOnlyInPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyInPa)[3] <- "epbc_sppOnlyInPa"

sppOnlyOutPa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")
sppOnlyOutPa <- setDT(sppOnlyOutPa)[, .(count = .N), keyby = c("biome", "YearRange")]
colnames(sppOnlyOutPa)[3] <- "epbc_sppOnlyOutPa"

fwrite(sppOnlyInPa, "cache/sumTable/biome/epbc_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/biome/epbc_sppOnlyOutPa.csv")

rm(epbc, sppOnlyInPa, sppOnlyOutPa, pa, pa1, pa2)

# epbc species distributed only inside (not outside) PA first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

pa <- epbc %>%
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
colnames(df_final)<- c("biome", "YearRange", "epbc_sppOnlyInPa_new_species",
                       "epbc_sppOnlyInPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc_sppOnlyInPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# epbc species distributed only outside (not inside) PA first/last seen count
epbc <- ala %>%
  dplyr::select("biome", "YearRange", "species_guid", "epbc_status", 
                "capad_status")

epbc <- epbc %>%
  dplyr::filter(epbc_status == "epbc")

pa <- epbc %>%
  dplyr::select(species_guid, biome, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "biome", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "biome", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "biome", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
pa <- setDT(pa)
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
colnames(df_final)<- c("biome", "YearRange", "epbc_sppOnlyOutPa_new_species",
                       "epbc_sppOnlyOutPa_last_seen_species")

fwrite(df_final, "cache/sumTable/biome/SpeciesFirst&LastObserved_epbc_sppOnlyOutPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa, pa1, pa2)