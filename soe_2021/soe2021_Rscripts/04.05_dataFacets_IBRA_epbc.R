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
sppByIbra1 <- sppByIbra1 %>% 
  dplyr::select(IBRA, YearRange, sppByIbra_epbc)

fwrite(sppByIbra1, "cache/sumTable/ibra/sppByIbra_epbc.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc.csv")

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

fwrite(sppInPa, "cache/sumTable/ibra/sppInIndPa_epbc.csv")

rm(epbc, pa, sppInPa)

# epbc species inside indigenous PA first/last seen count
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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc_sppInInd.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

# epbc species which are either inside (not outside) or outside (not inside) indigenous PAs
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

fwrite(sppOnlyInIndPa, "cache/sumTable/ibra/epbc_sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "cache/sumTable/ibra/epbc_sppOnlyOutIndPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc_sppOnlyInIndPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc_sppOnlyOutIndPa.csv")

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

fwrite(sppInPa, "cache/sumTable/ibra/sppInPa_epbc.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc_sppInPa.csv")

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

fwrite(sppOnlyInPa, "cache/sumTable/ibra/epbc_sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/ibra/epbc_sppOnlyOutPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc_sppOnlyInPa.csv")

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

fwrite(df_final, "cache/sumTable/ibra/SpeciesFirst&LastObserved_epbc_sppOnlyOutPa.csv")

rm(epbc, df_final, df_list, result_df, result_list, pa)

