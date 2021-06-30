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
griis_list <- fread("GRIIS_Aus.csv")

griis_ala_original <- select_taxa(griis_list$species)
griis_ala <- griis_ala_original %>%
  filter(match_type == "exactMatch", issues == "noIssue")

griis_ala <- griis_ala[!is.na(griis_ala$species), ]
griis <- merge(griis_list,
                  griis_ala[, c("search_term", "taxon_concept_id")],
                  by.x = "species", by.y = "search_term",
                  all = FALSE)

griis <- griis %>%
  dplyr::select(species, habitat, occurrenceStatus, establishmentMeans, isInvasive)
griis <- griis %>%
  mutate(griis_status = ifelse(isInvasive == "NULL", "introduced", "invasive"))

# Cross-checking WoNS dataset
wons_list <- fread("Weeds_of_National_Significance_(WoNS)_as_at_Feb._2013.csv")

wons_original <- select_taxa(wons_list$scientificName)
wons_ala <- wons_original %>%
  filter(match_type == "exactMatch", issues == "noIssue")

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
threatened_list <- fread("EPBC_list.csv")

threatened_ala <- select_taxa(threatened_list$scientific_name)
threatened_ala <- threatened_ala[!is.na(threatened_ala$species), ]
epbc <- merge(threatened_list,
                       threatened_ala[, c("search_term", "taxon_concept_id")],
                       by.x = "scientific_name", by.y = "search_term",
                       all = FALSE)

epbc <- epbc %>%
  dplyr::select(scientific_name, conservation_status, date_effective)

colnames(epbc) <- c("species", "conservation_status", "date_effective")
epbc <- epbc %>%
  dplyr::mutate(epbc = "Yes")

rm(threatened_ala, threatened_df, threatened_list, wons_ala, wons_list, wons_original,
   griis_list, griis_ala, griis_ala_original)

##########################################################################
# Terrestrial
files <- c("cache/intersect/Terrestrial/df1_intersect.csv", 
           "cache/intersect/Terrestrial/df2_intersect.csv", 
           "cache/intersect/Terrestrial/df3_intersect.csv", 
           "cache/intersect/Terrestrial/df4_intersect.csv", 
           "cache/intersect/Terrestrial/df5_intersect.csv", 
           "cache/intersect/Terrestrial/df6_intersect.csv", 
           "cache/intersect/Terrestrial/df7_intersect.csv", 
           "cache/intersect/Terrestrial/df8_intersect.csv", 
           "cache/intersect/Terrestrial/df9_intersect.csv", 
           "cache/intersect/Terrestrial/df10_intersect.csv", 
           "cache/intersect/Terrestrial/df11_intersect.csv",
           "cache/intersect/Terrestrial/df12_intersect.csv")

n_threads <- 12
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
  library(parallel)
  library(readr)
  library(plyr)
})
clusterExport(cl, c("griis", "wons", "epbc"))

# Main processing
result <- parLapply(cl, files, function(i) {
  
  ala <- fread(i, header = T)
  
  
  # Removing blank cells
  ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]
  
  colnames(ala)[18:23] <- paste(
    "CAPAD2020", 
    colnames(ala)[18:23],
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
  
  file <- gsub("cache/intersect/Terrestrial/", "", i)
  file <- gsub("_intersect.csv", "", file)
  
  fwrite(ala1, file = paste0("cache/Merged/Terrestrial/", file, "_merged.csv"), row.names = FALSE, quote = TRUE)
  
  rm(ala, ala1)
  
})

# Stop cluster
cl <- stopCluster(cl)

##########################################################################
# Marine
files <- c("cache/Intersect/Marine/df1_intersect.csv", 
           "cache/Intersect/Marine/df2_intersect.csv", 
           "cache/Intersect/Marine/df3_intersect.csv", 
           "cache/Intersect/Marine/df4_intersect.csv", 
           "cache/Intersect/Marine/df5_intersect.csv", 
           "cache/Intersect/Marine/df6_intersect.csv", 
           "cache/Intersect/Marine/df7_intersect.csv", 
           "cache/Intersect/Marine/df8_intersect.csv", 
           "cache/Intersect/Marine/df9_intersect.csv", 
           "cache/Intersect/Marine/df10_intersect.csv", 
           "cache/Intersect/Marine/df11_intersect.csv",
           "cache/Intersect/Marine/df12_intersect.csv")

n_threads <- 12
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
  library(parallel)
  library(readr)
  library(plyr)
})
clusterExport(cl, c("griis", "wons", "epbc"))

# Main processing
result <- parLapply(cl, files, function(i) {
  
  ala <- fread(i, header = T)
  
  
  # Removing blank cells
  ala <- ala[!(is.na(ala$species_guid) | ala$species_guid == ""),]
  
  colnames(ala)[19:23] <- paste(
    "CAPAD2020", 
    colnames(ala)[19:23],
    sep = "_")
  
  ala <- ala %>%
    dplyr::mutate(CAPAD_Status = ifelse(CAPAD2020_NAME == "Outside", "outside", "inside"))
  
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
  
  file <- gsub("cache/Intersect/Marine/", "", i)
  file <- gsub("_intersect.csv", "", file)
  
  fwrite(ala1, file = paste0("cache/Merged/Marine/", file, "_merged.csv"))
  
  rm(ala, ala1)
  
})

# Stop cluster
cl <- stopCluster(cl)
