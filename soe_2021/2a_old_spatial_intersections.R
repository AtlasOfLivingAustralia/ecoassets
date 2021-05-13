## Previous script for extracting CAPAD values at ALA occurrence points

states <- c("SA.csv", "WA.csv", "NT.csv", "TAS.csv", "ACT.csv",
            "NSW1.csv", "NSW2.csv", "NSW3.csv", "VIC1.csv", "VIC2.csv",
            "VIC3.csv", "QLD1.csv", "QLD2.csv", "QLD3.csv")

n_threads <- 14
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(ggplot2)
  library(sf)
  library(sp)
  library(parallel)
  library(foreach)
  library(readr)
})
clusterExport(cl, c("capad1"))

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
  occ.sp <- as(occ.sp, "sf")
  occ.sp <- st_make_valid(occ.sp)
  occ.sp <- st_set_crs(occ.sp, 4326)
  occ.sp1 <- occ.sp %>%
    dplyr::mutate(row.id = rownames(occ.sp))

  occ.sp1$row.id <- as.integer(occ.sp1$row.id)

  # Find which polygon each point is in
  intersect <- as.data.frame(st_intersects(occ.sp1, capad1))

  intersect1 <- merge(occ.sp1, intersect, by.x = "row.id", by.y = "row.id")
  intersect1 <- as.data.frame(intersect1)

  intersect2 <- merge(intersect1, capad1, by.x = "col.id", by.y = "col.id")
  intersect2 <- as.data.frame(intersect2)

  intersect3 <- intersect2 %>%
    dplyr::select(index, NAME, TYPE, IUCN, GIS_AREA, GAZ_DATE)

  merged <- merge(occ, intersect3, by = "index", all = TRUE)

  merged1 <- merged %>%
    dplyr::mutate(NAME = ifelse(is.na(NAME), "Outside", NAME))

  state <- gsub(".csv", "", i)

  write_csv(merged1, file = paste0("cache/", state, "_intersect.csv"), row.names = FALSE, quote = TRUE)

  saveRDS(merged1,
          paste0("cache/", state, "_intersect.rds"))

})

# Stop cluster
cl <- stopCluster(cl)


###########################
rm(list = ls())

NSW <- fread("NSW.csv")
head(NSW)

NSW1 <- NSW[1:5000000,]
NSW2 <- NSW[5000001:10000000,]
NSW3 <- NSW[10000001:14104689,]

write.csv(NSW1, "NSW1.csv")
write.csv(NSW2, "NSW2.csv")
write.csv(NSW3, "NSW3.csv")

rm(NSW, NSW1, NSW2, NSW3)

VIC <- fread("VIC.csv")

VIC1 <- VIC[1:5000000,]
VIC2 <- VIC[5000001:10000000,]
VIC3 <- VIC[10000001:13900171,]

write.csv(VIC1, "VIC1.csv")
write.csv(VIC2, "VIC2.csv")
write.csv(VIC3, "VIC3.csv")

rm(VIC, VIC1, VIC2, VIC3)

QLD <- fread("QLD.csv")

QLD1 <- QLD[1:5000000,]
QLD2 <- QLD[5000001:10000000,]
QLD3 <- QLD[10000001:10326744,]

write.csv(QLD1, "QLD1.csv")
write.csv(QLD2, "QLD2.csv")
write.csv(QLD3, "QLD3.csv")

rm(QLD, QLD1, QLD2, QLD3)

# Data inspection
act_int <- fread("cache/act_intersect.csv")
head(act_int)

rm(list = ls())

# Merging data
input_folder <- "cache/intersect/" # folder that contains all the csvs
data <- dir(input_folder, "^.*\\.csv$", full.names = TRUE) # create file names of all the csvs
ala <- plyr::ldply(data, data.table::fread)

colnames(ala)

colnames(ala)[17:21] <- paste(
  "CAPAD2020",
  colnames(ala)[17:21],
  sep = "_")

saveRDS(ala, file = paste0("cache/MergedIntersects.rds"))


# files <- c("file1", "file2")
# data_list <- lapply(files, function(a){fread(a)})
# data_final <- rbindlist(data_list)

names(ala)

ala <- ala[,2:21]

ala <- ala %>%
  dplyr::mutate(CAPAD_Status = ifelse(CAPAD2020_NAME == "Outside", "outside", "inside"))

griis <- fread("GRIIS_Aus.csv")

ala <- merge(ala, griis, by = "species", all = TRUE)

write_rds(ala, "ala_data.rds")


ala <- ala %>%
  dplyr::mutate(indigenous_Status = ifelse(CAPAD2020_TYPE ==
                                             c("Indigenous Protected Area", "National Park Aboriginal", "Aboriginal Area"),
                                           "inside", "outside"))
ala1 <- ala %>%
  dplyr::select(species, CAPAD_Status, indigenous_Status, isInvasive)
write_rds(ala1, "UpdatedAla.rds")

write_csv(ala1, "UpdatedALA.csv")
