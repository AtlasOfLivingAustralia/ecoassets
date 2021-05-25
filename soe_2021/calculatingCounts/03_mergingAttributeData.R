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

# Merging by
states <- c("cache/intersect/SA_intersect.csv", "cache/intersect/WA_intersect.csv", 
            "cache/intersect/NT_intersect.csv", 
            "cache/intersect/TAS_intersect.csv", "cache/intersect/ACT_intersect.csv",
            "cache/intersect/NSW1_intersect.csv", "cache/intersect/NSW2_intersect.csv", 
            "cache/intersect/NSW3_intersect.csv", "cache/intersect/VIC1_intersect.csv", 
            "cache/intersect/VIC2_intersect.csv", "cache/intersect/VIC3_intersect.csv", 
            "cache/intersect/QLD1_intersect.csv", "cache/intersect/QLD2_intersect.csv", 
            "cache/intersect/QLD3_intersect.csv")

n_threads <- 23
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
result <- parLapply(cl, states, function(i) {
  
  ala <- fread(i, header = T)
  
  colnames(ala)[16:21] <- paste(
    "CAPAD2020", 
    colnames(ala)[16:21],
    sep = "_")
  
  # Removing blank cells
  ala <- ala[!(is.na(ala$species) | ala$species == ""),]
  ala <- ala[!(is.na(ala$kingdom) | ala$kingdom == ""),]
  ala <- ala[!(is.na(ala$Australian.States.and.Territories) | ala$Australian.States.and.Territories == ""),]
  ala <- ala[!(is.na(ala$phylum) | ala$phylum == ""),]
  ala <- ala[!(is.na(ala$class) | ala$class == ""),]
  ala <- ala[!(is.na(ala$basisOfRecord) | ala$basisOfRecord == ""),]
  
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
  
  state <- gsub("cache/intersect/", "", i)
  state <- gsub("_intersect.csv", "", state)
  
  fwrite(ala1, file = paste0("merged/", state, "_merged.csv"), row.names = FALSE, quote = TRUE)
  
  rm(ala, ala1)
  
})

# Stop cluster
cl <- stopCluster(cl)

# It took about 14 minutes to finish the calculations


# When exporting intersection data by states, there was an additional column 'X' in the QLD, NSW, and VIC, but not in the other states. 
# For this, when we renamed columns, an error occurred. We are fixing this issue here.

states <- c("merged/NRM/NSW1_nrm_intersect.csv", "merged/NRM/NSW2_nrm_intersect.csv", 
            "merged/NRM/NSW3_nrm_intersect.csv", "merged/NRM/VIC1_nrm_intersect.csv", 
            "merged/NRM/VIC2_nrm_intersect.csv", "merged/NRM/VIC3_nrm_intersect.csv", 
            "merged/NRM/QLD1_nrm_intersect.csv", "merged/NRM/QLD2_nrm_intersect.csv", 
            "merged/NRM/QLD3_nrm_intersect.csv")

n_threads <- 9
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
})
clusterExport(cl, c("states"))

# Main processing
result <- try(parLapply(cl, states, function(i) {
  df <- fread(i)
  
  colnames(df)[16] <- c("IBRA.7.Regions")
  
  state <- gsub("_nrm_intersect.csv", "", i)
  state <- gsub("merged/NRM/", "", state)
  
  fwrite(df, file = paste0("merged/NRM/", state, "_nrm_intersect.csv"), row.names = FALSE, quote = TRUE)
  
}), silent = FALSE)

# Stop cluster
cl <- stopCluster(cl)


# Merging state-wise files
QLD1 <- fread("merged/NRM/QLD1_nrm_intersect.csv")
QLD1 <- fread("merged/NRM/QLD1_nrm_intersect.csv")
QLD1 <- fread("merged/NRM/QLD1_nrm_intersect.csv")
QLD <- rbind(QLD1, QLD2, QLD3)
fwrite(QLD, "merged/NRM/QLD_nrm_intersect.csv")
rm(QLD1, QLD2, QLD3, QLD)

QLD1 <- fread("merged/NRM/QLD1_nrm_intersect.csv")
QLD2 <- fread("merged/NRM/QLD2_nrm_intersect.csv")
QLD3 <- fread("merged/NRM/QLD3_nrm_intersect.csv")
QLD <- rbind(QLD1, QLD2, QLD3)
fwrite(QLD, "merged/NRM/QLD_nrm_intersect.csv")
rm(QLD1, QLD2, QLD3, QLD)

NSW1 <- fread("merged/NRM/NSW1_nrm_intersect.csv")
NSW2 <- fread("merged/NRM/NSW2_nrm_intersect.csv")
NSW3 <- fread("merged/NRM/NSW3_nrm_intersect.csv")
NSW <- rbind(NSW1, NSW2, NSW3)
fwrite(NSW, "merged/NRM/NSW_nrm_intersect.csv")
rm(NSW1, NSW2, NSW3, NSW)

VIC1 <- fread("merged/NRM/VIC1_nrm_intersect.csv")
VIC2 <- fread("merged/NRM/VIC2_nrm_intersect.csv")
VIC3 <- fread("merged/NRM/VIC3_nrm_intersect.csv")
VIC <- rbind(VIC1, VIC2, VIC3)
fwrite(VIC, "merged/NRM/VIC_nrm_intersect.csv")
rm(VIC1, VIC2, VIC3, VIC)