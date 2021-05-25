options(java.parameters = "-Xmx6g")

library(data.table)
library(dplyr)
library(sf)
library(sp)
library(parallel)
library(readr)
library(plyr)

# Intersecting with the NRM layer
nrm <- st_read("nrm/CM_Zones.shp")
nrm <- st_transform(nrm, 4326)
nrm <- st_make_valid(nrm)
nrm1 <- nrm %>%
  dplyr::mutate(col.id = rownames(nrm))

nrm1$col.id <- as.integer(nrm1$col.id)


states <- c("merged/SA_merged.csv", "merged/WA_merged.csv", 
            "merged/TAS_merged.csv", "merged/ACT_merged.csv",
            "merged/NSW1_merged.csv", "merged/NSW2_merged.csv", 
            "merged/NSW3_merged.csv", "merged/VIC1_merged.csv", 
            "merged/VIC2_merged.csv", "merged/VIC3_merged.csv", 
            "merged/QLD1_merged.csv", "merged/QLD2_merged.csv", 
            "merged/QLD3_merged.csv", "merged/NT_merged.csv")

states <- c("merged/QLD3_merged.csv")

n_threads <- 4
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
  library(sf)
  library(sp)
  library(parallel)
  library(readr)
  library(plyr)
})
clusterExport(cl, c("nrm1"))

# Main processing
result <- parLapply(cl, states, function(i) {
  
  occ <- fread(i, header = T)
  occ <- occ %>%
    dplyr::mutate(index = as.integer(rownames(occ)))
  
  # Filter records not on the mainland/tasmania
  filtered_occ <- occ %>% dplyr::select(decimalLatitude, decimalLongitude, index) %>%
    dplyr::filter(decimalLongitude < 155,
                  decimalLongitude > 110,
                  decimalLatitude > -45,
                  decimalLatitude < -10)
  
  # Remove records without coordinates
  filtered_occ1 <- filtered_occ %>%
    dplyr::filter(!is.na(decimalLongitude)) %>%
    dplyr::filter(!is.na(decimalLatitude))
  
  # Adding ID
  filtered_occ1$id <- rep(1:length(filtered_occ1$index))
  
  # Converting to spatial object
  occ.sp <- SpatialPointsDataFrame(filtered_occ1[,c(2,1)], filtered_occ1[,-c(2,1)])
  
  rm(filtered_occ, filtered_occ1)
  
  occ.sp <- as(occ.sp, "sf")
  occ.sp <- st_make_valid(occ.sp)
  occ.sp <- st_set_crs(occ.sp, 4326)
  occ.sp <- occ.sp %>%
    dplyr::mutate(row.id = rownames(occ.sp))
  
  occ.sp$row.id <- as.integer(occ.sp$row.id)
  
  # Find which polygon each point is in
  intersect <- as.data.frame(st_intersects(occ.sp, nrm1))
  
  intersect <- merge(occ.sp, intersect, by.x = "row.id", by.y = "row.id")
  intersect <- as.data.frame(intersect)
  
  intersect <- merge(intersect, nrm1, by.x = "col.id", by.y = "col.id")
  intersect <- as.data.frame(intersect)
  
  intersect <- intersect %>%
    dplyr::select(index, CM_ZONE, CMZ_AREA_S)
  
  merged <- merge(occ, intersect, by = "index", all = TRUE)
  
  state <- gsub("_merged.csv", "", i)
  state <- gsub("merged/", "", state)
  
  fwrite(merged, file = paste0("merged/NRM/", state, "_nrm_intersect.csv"), row.names = FALSE, quote = TRUE)
  
  rm(merged)
  
})

# Stop cluster
cl <- stopCluster(cl)