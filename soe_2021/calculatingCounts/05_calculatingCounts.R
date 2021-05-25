options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)

# Calculating counts by columns
states <- c("merged/NRM/NSW_nrm_intersect.csv", "merged/NRM/VIC_nrm_intersect.csv", 
            "merged/NRM/QLD_nrm_intersect.csv", "merged/NRM/SA_nrm_intersect.csv", 
            "merged/NRM/WA_nrm_intersect.csv", "merged/NRM/TAS_nrm_intersect.csv", 
            "merged/NRM/ACT_nrm_intersect.csv","merged/NRM/NT_nrm_intersect.csv")


n_threads <- 16
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
})
clusterExport(cl, c("states"))

# Main processing
result <- try(parLapply(cl, states, function(i) {
  df <- fread(i)
  
  # Removing duplicates
  setkey(df,NULL)
  df1 <- unique(df)
  
  # Removing blank cells
  df1 <- df1[!(is.na(df1$species_guid) | df1$species_guid == ""),]
  
  # Subsetting data
  df1 <- df1 %>%
    dplyr::select(species_guid, year, basisOfRecord, Australian.States.and.Territories,
                  IBRA.7.Regions, CAPAD2020_NAME, wons_status, griis_status,
                  conservation_status, CM_ZONE)
  
  # Renaming some columns
  colnames(df1) <- c("species_guid", "year", "basisOfRecord", "States&Territories",
                     "IBRA.7.Regions", "CAPAD2020_NAME", "wons_status", "griis_status",
                     "conservation_status", "ConservManageZone")
  
  df1 <- df1[, `:=` (count = .N), by = species_guid]
  
  # Removing duplicates
  setkey(df1,NULL)
  df1 <- unique(df1)
  
  state <- gsub("_nrm_intersect.csv", "", i)
  state <- gsub("merged/NRM/", "", state)
  
  fwrite(df1, file = paste0("merged/count/", state, "_count.csv"), row.names = FALSE, quote = TRUE)
  
}), silent = FALSE)

# Stop cluster
cl <- stopCluster(cl)

# It took about 4 minutes.