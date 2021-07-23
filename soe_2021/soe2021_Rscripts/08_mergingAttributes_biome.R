options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)
library(sf)
library(sp)
library(parallel)
library(readr)
library(galah)
library(plyr)

# Cross-checking GRIIS dataset
griis_list <- fread("cache/otherAttributes/griis.csv")
griis_ala_original <- select_taxa(griis_list$species)
griis_ala <- subset(griis_ala_original, (match_type == "exactMatch") & (issues == "noIssue"))
griis_ala <- griis_ala[!is.na(griis_ala$species), ]
griis <- merge(griis_list,
               griis_ala[, c("search_term", "taxon_concept_id")],
               by.x = "species", by.y = "search_term",
               all = FALSE)
griis <- griis %>%
  dplyr::select(species, habitat, occurrenceStatus, establishmentMeans, isInvasive)
griis <- griis %>%
  dplyr::mutate(griis_status = ifelse(isInvasive == "NULL", "introduced", "invasive"))

# Cross-checking WoNS dataset
wons_list <- fread("cache/otherAttributes/wons.csv")
wons_original <- select_taxa(wons_list$scientificName)
wons_ala <- subset(wons_original, (match_type == "exactMatch") & (issues == "noIssue"))
wons_ala <- wons_ala[!is.na(wons_ala$species), ]
wons <- merge(wons_list,
              wons_ala[, c("search_term", "taxon_concept_id")],
              by.x = "scientificName", by.y = "search_term",
              all = FALSE)
wons <- wons %>%
  dplyr::select(scientificName)
colnames(wons) <- c("species")
wons <- wons %>%
  dplyr::mutate(WoNS = "WoNS")
wons <- wons[wons$species != "", ]

# Cross-checking EPBC list
threatened_list <- fread("cache/otherAttributes/epbc.csv")
# threatened_ala <- select_taxa(threatened_list$scientific_name)
# threatened_ala <- threatened_ala[!is.na(threatened_ala$species), ]
# epbc <- merge(threatened_list,
#                        threatened_ala[, c("search_term", "taxon_concept_id")],
#                        by.x = "scientific_name", by.y = "search_term",
#                        all = FALSE)

epbc <- threatened_list %>%
  dplyr::select(scientific_name, conservation_status, date_effective)
colnames(epbc) <- c("species", "conservation_status", "date_effective")
epbc <- epbc %>%
  dplyr::mutate(epbc = "Yes")

rm(threatened_ala, threatened_df, threatened_list, wons_ala, wons_list, wons_original,
   griis_list, griis_ala, griis_ala_original)
##########################################################################
# Terrestrial
files <- c("cache/intersect/biome/df1_intersect_biome.csv",
           "cache/intersect/biome/df2_intersect_biome.csv",
           "cache/intersect/biome/df3_intersect_biome.csv",
           "cache/intersect/biome/df4_intersect_biome.csv",
           "cache/intersect/biome/df5_intersect_biome.csv",
           "cache/intersect/biome/df6_intersect_biome.csv",
           "cache/intersect/biome/df7_intersect_biome.csv",
           "cache/intersect/biome/df8_intersect_biome.csv",
           "cache/intersect/biome/df9_intersect_biome.csv",
           "cache/intersect/biome/df10_intersect_biome.csv",
           "cache/intersect/biome/df11_intersect_biome.csv",
           "cache/intersect/biome/df12_intersect_biome.csv",
           "cache/intersect/biome/df13_intersect_biome.csv",
           "cache/intersect/biome/df14_intersect_biome.csv")

for(i in files) {

  ala <- fread(i, header = T)

  colnames(ala)[19:23] <- paste(
    "CAPAD2020",
    colnames(ala)[19:23],
    sep = "_")

  ala <- ala %>%
    dplyr::mutate(CAPAD_Status = ifelse(CAPAD2020_NAME == "Outside", "outside", "inside"))

  ala <- ala %>%
    dplyr::mutate(indigenous_Status = ifelse(CAPAD2020_TYPE %in%
                                               c("Indigenous Protected Area", "National Park Aboriginal", "Aboriginal Area"),
                                             "inside", "outside"))

  # Merging with the GRIIS dataset
  ala1 <- merge(ala, griis, by = "species", all = TRUE, allow.cartesian = TRUE)

  ala1 <- ala1 %>%
    mutate(griis_status = ifelse(is.na(isInvasive), "other", isInvasive),
           habitat = ifelse(is.na(habitat), "other", habitat),
           occurrenceStatus = ifelse(is.na(occurrenceStatus), "other", occurrenceStatus),
           establishmentMeans = ifelse(is.na(establishmentMeans), "other", establishmentMeans),
           isInvasive = ifelse(is.na(isInvasive), "other", isInvasive))


  # Merging with the EPBC list
  ala1 <- merge(ala1, epbc, by = "species", all = TRUE, allow.cartesian = TRUE)

  ala1 <- ala1 %>%
    mutate(conservation_status = ifelse(is.na(conservation_status), "Non-threatened", conservation_status),
           date_effective = ifelse(is.na(date_effective), "Non-threatened", date_effective),
           epbc = ifelse(is.na(epbc), "Non-threatened", epbc))

  # Merging with WoNS
  ala1 <- merge(ala1, wons, by = "species", all = TRUE, allow.cartesian = TRUE)

  ala1 <- ala1 %>%
    mutate(wons_status = ifelse(is.na(WoNS), "Other", WoNS),
           WoNS = ifelse(is.na(WoNS), "Other", WoNS))

  ala1 <- ala1 %>%
    dplyr::select(speciesID, year, basisOfRecord,
                  B_NAME, CAPAD_Status, wons_status, griis_status,
                  conservation_status, NRM, indigenous_Status)
  
  file <- gsub("cache/intersect/biome/", "", i)
  file <- gsub("_intersect_biome.csv", "", file)

  fwrite(ala1, file = paste0("cache/merged/biome/", file, "_merged.csv"), row.names = FALSE, quote = TRUE)

  rm(ala, ala1)

}
