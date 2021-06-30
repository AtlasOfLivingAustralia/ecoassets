# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(sf)
library(sp)
library(parallel)
library(readr)

# Intersecting with the CAPAD 2020 layer (terrestrial)
# Reading and validating shapefile
capad <- st_read("CAPAD/CAPAD2020_terrestrial.shp")
capad <- st_transform(capad, 4326)
capad <- st_make_valid(capad)
capad <- st_collection_extract(capad, "POLYGON")

# We are creating an additional column that we will need later to merge with the occurrence data.
capad1 <- capad %>%
  dplyr::mutate(col.id = rownames(capad))

capad1$col.id <- as.integer(capad1$col.id)

# Intersection by smaller data files to accelerate the process
files <- c("cache/SmallerChunks/df1.csv", "cache/SmallerChunks/df2.csv", 
           "cache/SmallerChunks/df3.csv", "cache/SmallerChunks/df4.csv", 
           "cache/SmallerChunks/df5.csv", "cache/SmallerChunks/df6.csv",
           "cache/SmallerChunks/df7.csv", "cache/SmallerChunks/df8.csv", 
           "cache/SmallerChunks/df9.csv", "cache/SmallerChunks/df10.csv", 
           "cache/SmallerChunks/df11.csv", "cache/SmallerChunks/df12.csv")

n_threads <- 14 # number of threads depend on the computer configuration
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
  library(sf)
  library(sp)
  library(readr)
})
clusterExport(cl, c("capad1"))

# Main processing
result <- parLapply(cl, files, function(i) {
  
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
  
  file <- gsub(".csv", "", i)
  file <- gsub("cache/SmallerChunks/", "", file)
  
  fwrite(merged1, file = paste0("cache/Intersect/Terrestrial/", file, "_intersect.csv"))
  
})

# Stop cluster
cl <- stopCluster(cl)

###########################################
rm(list = ls())
# Intersecting with the CAPAD 2020 layer (marine)
# Reading and validating shapefile
capad <- st_read("CAPAD/CAPAD2020_marine.shp")
capad <- st_transform(capad, 4326)
capad <- st_make_valid(capad)
capad <- st_collection_extract(capad, "POLYGON")

# We are creating an additional column that we will need later to merge with the occurrence data.
capad1 <- capad %>%
  dplyr::mutate(col.id = rownames(capad))

capad1$col.id <- as.integer(capad1$col.id)

# Intersection by smaller data files to accelerate the process
files <- c("cache/SmallerChunks/df1.csv", "cache/SmallerChunks/df2.csv", 
           "cache/SmallerChunks/df3.csv", "cache/SmallerChunks/df4.csv", 
           "cache/SmallerChunks/df5.csv", "cache/SmallerChunks/df6.csv",
           "cache/SmallerChunks/df7.csv", "cache/SmallerChunks/df8.csv", 
           "cache/SmallerChunks/df9.csv", "cache/SmallerChunks/df10.csv", 
           "cache/SmallerChunks/df11.csv", "cache/SmallerChunks/df12.csv")


n_threads <- 6 # number of threads depend on the computer configuration
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(data.table)
  library(dplyr)
  library(sf)
  library(sp)
  library(readr)
})
clusterExport(cl, c("capad1"))

# Main processing
result <- parLapply(cl, files, function(i) {
  
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
  
  merged1 <- merged1[!(is.na(merged1$IMCRA) | merged1$IMCRA == ""),]
  
  file <- gsub(".csv", "", i)
  file <- gsub("cache/SmallerChunks/", "", file)
  
  fwrite(merged1, file = paste0("cache/Intersect/Marine/", file, "_intersect.csv"))
  
})

# Stop cluster
cl <- stopCluster(cl)
