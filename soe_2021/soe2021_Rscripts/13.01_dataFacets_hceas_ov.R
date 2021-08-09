# For all speies 
# Occurrence records count
occBylandUseZone <- ala %>%
  dplyr::select("landUseZone", "YearRange")
occBylandUseZone <- setDT(occBylandUseZone)[, .(occBylandUseZone = .N), keyby = c("landUseZone", "YearRange")]

fwrite(occBylandUseZone, "cache/sumTable/landUseZone/occBylandUseZone.csv")

rm(occBylandUseZone)

# Species count
sppBylandUseZone <- ala %>%
  dplyr::select("species_guid", "landUseZone", "YearRange")

# Removing duplicates
sppBylandUseZone <- setDT(sppBylandUseZone)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange")]
sppBylandUseZone <- sppBylandUseZone %>% 
  dplyr::select("species_guid", "landUseZone", "YearRange")

# Calculating count
sppBylandUseZone <- setDT(sppBylandUseZone)[, .(sppBylandUseZone = .N), keyby = c("landUseZone", "YearRange")]
fwrite(sppBylandUseZone, "cache/sumTable/landUseZone/sppBylandUseZone.csv")
rm(sppBylandUseZone)

# Number of citizen science records
cs <- ala %>%
  dplyr::select(landUseZone, YearRange, basisOfRecord)

cs <- cs %>%
  dplyr::mutate(basisOfRecord = ifelse(basisOfRecord == "HUMAN_OBSERVATION", "CitizenScience", "OtherSource"))
cs <- cs %>%
  dplyr::filter(basisOfRecord == "CitizenScience")

cs <- setDT(cs)[, .(CS_records = .N), keyby = c("landUseZone", "YearRange", "basisOfRecord")]
cs <- cs %>%
  dplyr::select(landUseZone, YearRange, CS_records)

fwrite(cs, "cache/sumTable/landUseZone/NumberOfCitizenScienceRecords.csv")
rm(cs)

# Species first/last seen count
ala_simple <- setDT(ala)[,.(.N), c("landUseZone", "YearRange", "species_guid")]

ala_simple$YearRange <- as.numeric(ala_simple$YearRange)
df_final <- ala_simple[, .(.N), keyby = c("landUseZone", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
  last_seen_species <- length(which(!(current_year_species %in% later_year_species)))
  cumulative_species <-length(unique(
    x[landUseZone == a$landUseZone & YearRange <= a$YearRange, "species_guid"]$species_guid
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved.csv")

rm(ala_simple, df_final, df_list, result_df, result_list, data, input_folder)

# Number of species inside indigenous PA
pa <- ala %>%
  dplyr::select(landUseZone, species_guid, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(count = .N), keyby = c("landUseZone", "species_guid", "YearRange", "indigenous_Status")]
pa <- pa %>% 
  dplyr::select("landUseZone", "species_guid", "YearRange", "indigenous_Status")

pa <- setDT(pa)[, .(count = .N), keyby = c("landUseZone", "YearRange", "indigenous_Status")]

sppInPa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")
sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, count)
colnames(sppInPa)[3] <- "sppInIndPa"

sppOutPa <- pa %>%
  dplyr::filter(indigenous_Status == "outside")
sppOutPa <- sppOutPa %>%
  dplyr::select(landUseZone, YearRange, count)
colnames(sppOutPa)[3] <- "sppOutIndPa"

fwrite(sppInPa, "cache/sumTable/landUseZone/sppInIndPa.csv")
fwrite(sppOutPa, "cache/sumTable/landUseZone/sppOutIndPa.csv")

rm(pa, sppInPa, sppOutPa)

# Species inside indigenous pa first/last seen count
pa <- ala %>%
  dplyr::select(landUseZone, species_guid, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(count = .N), keyby = c("landUseZone", "species_guid", "YearRange", "indigenous_Status")]
pa <- pa %>% 
  dplyr::select("landUseZone", "species_guid", "YearRange", "indigenous_Status")
pa <- pa %>%
  dplyr::filter(indigenous_Status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("landUseZone", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("landUseZone", "YearRange", "sppInIndPa_new_species",
                       "sppInIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved_sppInIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species inside PA count
pa <- ala %>%
  dplyr::select(landUseZone, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)

pa <- setDT(pa)[, .(sppInPa = .N), keyby = c("landUseZone", "YearRange", "capad_status")]

sppInPa <- pa %>%
  dplyr::filter(capad_status == "inside")

sppInPa <- sppInPa %>%
  dplyr::select(landUseZone, YearRange, sppInPa)

fwrite(sppInPa, "cache/sumTable/landUseZone/sppInPa.csv")

rm(pa, sppInPa)

# Species inside PA first/last seen count
pa <- ala %>%
  dplyr::select(landUseZone, species_guid, YearRange, capad_status)

# Removing duplicates
setkey(pa,NULL)
pa <- unique(pa)
pa <- pa %>%
  dplyr::filter(capad_status == "inside")

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("landUseZone", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("landUseZone", "YearRange", "sppInPa_new_species",
                       "sppInPa_last_seen_species")

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved_sppInPa.csv")

rm(df_final, df_list, result_df, result_list, pa)

# Species which are either inside (not outside) or outside (not inside) PAs
pa <- ala %>%
  dplyr::select(species_guid, landUseZone, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "landUseZone", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "landUseZone", "YearRange"))

sppOnlyInPa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")
sppOnlyInPa <- setDT(sppOnlyInPa)[, .(count = .N), keyby = c("landUseZone", "YearRange")]
colnames(sppOnlyInPa)[3] <- "sppOnlyInPa"

sppOnlyOutPa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")
sppOnlyOutPa <- setDT(sppOnlyOutPa)[, .(count = .N), keyby = c("landUseZone", "YearRange")]
colnames(sppOnlyOutPa)[3] <- "sppOnlyOutPa"

fwrite(sppOnlyInPa, "cache/sumTable/landUseZone/sppOnlyInPa.csv")
fwrite(sppOnlyOutPa, "cache/sumTable/landUseZone/sppOnlyOutPa.csv")

rm(pa, pa1, pa2, sppOnlyInPa, sppOnlyOutPa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(species_guid, landUseZone, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "landUseZone", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "landUseZone", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "inside")

pa <- pa %>%
  dplyr::select(species_guid, landUseZone, YearRange)

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("landUseZone", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("landUseZone", "YearRange", "sppOnlyInPa_new_species",
                       "sppOnlyInPa_last_seen_species")

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved_sppOnlyInPa.csv")

rm(df_final, df_list, result_df, result_list, pa, pa1, pa2)

# Species distributed only outside (not inside) PA first/last seen count
pa <- ala %>%
  dplyr::select(species_guid, landUseZone, YearRange, capad_status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange", "capad_status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "landUseZone", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "landUseZone", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & capad_status == "outside")

pa <- pa %>%
  dplyr::select(species_guid, landUseZone, YearRange)

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("landUseZone", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("landUseZone", "YearRange", "sppOnlyOutPa_new_species",
                       "sppOnlyOutPa_last_seen_species")

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved_sppOnlyOutPa.csv")

rm(df_final, df_list, result_df, result_list, pa, pa1, pa2)

# Species which are either inside (not outside) or outside (not inside) indigenous PAs
pa <- ala %>%
  dplyr::select(species_guid, landUseZone, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "landUseZone", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "landUseZone", "YearRange"))

sppOnlyInIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")
sppOnlyInIndPa <- setDT(sppOnlyInIndPa)[, .(count = .N), keyby = c("landUseZone", "YearRange")]
colnames(sppOnlyInIndPa)[3] <- "sppOnlyInIndPa"

sppOnlyOutIndPa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")
sppOnlyOutIndPa <- setDT(sppOnlyOutIndPa)[, .(count = .N), keyby = c("landUseZone", "YearRange")]
colnames(sppOnlyOutIndPa)[3] <- "sppOnlyOutIndPa"

fwrite(sppOnlyInIndPa, "cache/sumTable/landUseZone/sppOnlyInIndPa.csv")
fwrite(sppOnlyOutIndPa, "cache/sumTable/landUseZone/sppOnlyOutIndPa.csv")

rm(pa, pa1, pa2, sppOnlyInIndPa, sppOnlyOutIndPa)

# Species distributed only inside (not outside) PA first/last seen count
pa <- ala %>%
  dplyr::select(species_guid, landUseZone, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "landUseZone", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "landUseZone", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "inside")

pa <- pa %>%
  dplyr::select(species_guid, landUseZone, YearRange)

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("landUseZone", "YearRange")]


df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("landUseZone", "YearRange", "sppOnlyInIndPa_new_species",
                       "sppOnlyInIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved_sppOnlyInIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa, pa1, pa2)

# Species distributed only outside (not inside) PA first/last seen count
pa <- ala %>%
  dplyr::select(species_guid, landUseZone, YearRange, indigenous_Status)

# Removing duplicates
pa <- setDT(pa)[, .(n = .N), keyby = c("species_guid", "landUseZone", "YearRange", "indigenous_Status")]

pa1 <- setDT(pa)[, .(count = .N), keyby = c("species_guid", "landUseZone", "YearRange")]

pa2 <- pa1 %>% 
  left_join(pa, by = c("species_guid", "landUseZone", "YearRange"))

pa <- pa2 %>% 
  filter(count == 1 & indigenous_Status == "outside")

pa <- pa %>%
  dplyr::select(species_guid, landUseZone, YearRange)

pa$YearRange <- as.numeric(pa$YearRange)
df_final <- pa[, .(.N), keyby = c("landUseZone", "YearRange")]

df_list <- split(df_final, seq_len(nrow(df_final)))
result_list <- lapply(df_list, function(a, x){
  current_year_species <- x[landUseZone == a$landUseZone & YearRange == a$YearRange, "species_guid"]$species_guid
  previous_year_species <- unique(x[landUseZone == a$landUseZone & YearRange < a$YearRange, "species_guid"]$species_guid)
  new_species <- length(which(!(current_year_species %in% previous_year_species)))
  later_year_species <-  unique(x[landUseZone == a$landUseZone & YearRange > a$YearRange, "species_guid"]$species_guid)
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
  dplyr::select(landUseZone, YearRange, new_species, last_seen_species)
colnames(df_final)<- c("landUseZone", "YearRange", "sppOnlyOutIndPa_new_species",
                       "sppOnlyOutIndPa_last_seen_species")

fwrite(df_final, "cache/sumTable/landUseZone/SpeciesFirst&LastObserved_sppOnlyOutIndPa.csv")

rm(df_final, df_list, result_df, result_list, pa, pa1, pa2)
