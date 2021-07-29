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
sppByIbra1 <- sppByIbra1 %>% 
  dplyr::select(IBRA, YearRange, sppByIbra_wons)

fwrite(sppByIbra1, "cache/sumTable/ibra/sppByIbra_wons.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS.csv")

rm(df_final, df_list, result_df, result_list, wons)

# Number of WoNS species inside indigenous PA
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

fwrite(sppInPa, "cache/sumTable/ibra/sppInIndPa_WoNS.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS_sppInInd.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

# WoNS species which are either inside (not outside) or outside (not inside) indigenous PAs
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", 
                "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

pa <- wons %>%
  dplyr::select(species_guid, IBRA, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IBRA", "YearRange"))

sppOnlyInIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")
sppOnlyInIndPa <- sppOnlyInIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInIndPa)[3] <- "WoNS_sppOnlyInIndPa"

sppOnlyOutIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")
sppOnlyOutIndPa <- sppOnlyOutIndPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutIndPa)[3] <- "WoNS_sppOnlyOutIndPa"

fwrite(sppOnlyInPa, "cache/sumTable/ibra/WoNS_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/ibra/WoNS_sppOnlyOutIndPa.csv")

rm(wons, Ind_pa, sppOnlyInIndPa, sppOnlyOutIndPa)

# WoNS species distributed only inside (not outside) indigenous PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

pa <- wons %>%
  dplyr::select(species_guid, IBRA, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IBRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS_sppOnlyInIndPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# WoNS species distributed only outside (not inside) indigenous PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "indigenous_Status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

pa <- wons %>%
  dplyr::select(species_guid, IBRA, YearRange, indigenous_Status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IBRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
pa <- setDT(pa)
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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS_sppOnlyOutIndPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa, pa1, pa2)


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

fwrite(sppInPa, "cache/sumTable/ibra/sppInPa_WoNS.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS_sppInPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa)

# WoNS species which are either inside (not outside) or outside (not inside) PAs
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", 
                "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

pa <- wons %>%
  dplyr::select(species_guid, IBRA, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IBRA", "YearRange"))

sppOnlyInPa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")
sppOnlyInPa <- sppOnlyInPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyInPa)[3] <- "WoNS_sppOnlyInPa"

sppOnlyOutPa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")
sppOnlyOutPa <- sppOnlyOutPa %>%
  dplyr::select(IBRA, YearRange, count)
colnames(sppOnlyOutPa)[3] <- "WoNS_sppOnlyOutPa"

fwrite(sppOnlyInPa, "cache/sumTable/ibra/WoNS_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/ibra/WoNS_sppOnlyOutPa.csv")

rm(wons, Ind_pa, sppOnlyInPa, sppOnlyOutPa)

# WoNS species distributed only inside (not outside) PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

pa <- wons %>%
  dplyr::select(species_guid, IBRA, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IBRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS_sppOnlyInPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa, pa1, pa2)

# WoNS species distributed only outside (not inside) PA first/last seen count
wons <- ala %>%
  dplyr::select("IBRA", "YearRange", "species_guid", "wons_status", "capad_status")

wons <- wons %>%
  dplyr::filter(wons_status == "WoNS")

pa <- wons %>%
  dplyr::select(species_guid, IBRA, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "IBRA", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "IBRA", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")

pa$YearRange <- as.numeric(pa$YearRange)
pa <- setDT(pa)
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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_WoNS_sppOnlyOutPa.csv")

rm(wons, df_final, df_list, result_df, result_list, pa, pa1, pa2)