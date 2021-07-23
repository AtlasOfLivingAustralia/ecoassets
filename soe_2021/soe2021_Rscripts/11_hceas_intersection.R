# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(sf)
library(sp)

# Reading and validating shapefile
hceas <- st_read("cache/shapeFiles/landUseZone/landUseZone.shp")
hceas <- st_transform(hceas, 4326)
hceas <- st_make_valid(hceas)
#hceas <- st_collection_extract(hceas, "POLYGON")

# We are creating an additional column that we will need later to merge with the occurrence data.
hceas1 <- hceas %>%
  dplyr::mutate(col.id = rownames(hceas))

hceas1$col.id <- as.integer(hceas1$col.id)

# Removing NA values
hceas1 <- hceas1[!(is.na(hceas1$LandUseZon) | hceas1$LandUseZon == ""),]


# Intersection by smaller data files to accelerate the process
files <- c("cache/intersect/terrestrial/df1_intersect.csv",
           "cache/intersect/terrestrial/df2_intersect.csv",
           "cache/intersect/terrestrial/df3_intersect.csv",
           "cache/intersect/terrestrial/df4_intersect.csv",
           "cache/intersect/terrestrial/df5_intersect.csv",
           "cache/intersect/terrestrial/df6_intersect.csv",
           "cache/intersect/terrestrial/df7_intersect.csv",
           "cache/intersect/terrestrial/df8_intersect.csv",
           "cache/intersect/terrestrial/df9_intersect.csv",
           "cache/intersect/terrestrial/df10_intersect.csv",
           "cache/intersect/terrestrial/df11_intersect.csv",
           "cache/intersect/terrestrial/df12_intersect.csv",
           "cache/intersect/terrestrial/df13_intersect.csv",
           "cache/intersect/terrestrial/df14_intersect.csv")


for(i in files) {
  
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
  intersect <- as.data.frame(st_intersects(occ.sp1, hceas1))
  
  intersect1 <- merge(occ.sp1, intersect, by.x = "row.id", by.y = "row.id")
  intersect1 <- as.data.frame(intersect1)
  
  intersect2 <- merge(intersect1, hceas1, by.x = "col.id", by.y = "col.id")
  intersect2 <- as.data.frame(intersect2)
  
  intersect3 <- intersect2 %>%
    dplyr::select(index, LandUseZon)
  
  merged <- merge(occ, intersect3, by = "index", all = TRUE)
  
  file <- gsub(".csv", "", i)
  file <- gsub("cache/intersect/terrestrial/", "", file)
  
  fwrite(merged, file = paste0("cache/intersect/landUseZone/", file, "_landUseZone.csv"))
  
}
