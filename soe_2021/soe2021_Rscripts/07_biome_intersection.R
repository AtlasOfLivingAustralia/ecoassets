# Here, we have used outputs from '02_capadIntersection.R'.

# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(sf)
library(sp)


###########################################
# Intersecting with the CAPAD 2020 layer (terrestrial)
# Reading and validating shapefile
biome <- st_read("cache/shapeFiles/biome/biome.shp")
biome <- st_transform(biome, 4326)
biome <- st_make_valid(biome)
#biome <- st_collection_extract(biome, "POLYGON")

# We are creating an additional column that we will need later to merge with the occurrence data.
biome1 <- biome %>%
  dplyr::mutate(col.id = rownames(biome))

biome1$col.id <- as.integer(biome1$col.id)
# 
# # Reading IBRA layer
# ibra <- st_read("cache/shapeFiles/ibra/ibra.shp")
# ibra <- st_transform(ibra, 4326)
# ibra <- st_make_valid(ibra)
# 
# # Intersection between biome and ibra to find out which ibra is within which biome
# int <- st_intersection(ibra, biome)
# 
# # Subsetting the data frame
# int1 <- int %>% 
#   dplyr::select(REG_NAME_7, B_NAME)
# 
# # Removing geometry
# int1$geometry <- NULL
# 
# # Writing csv
# fwrite(int1, "cache/intersect/biome_ibra_int.csv")

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
  intersect <- as.data.frame(st_intersects(occ.sp1, biome1))

  intersect1 <- merge(occ.sp1, intersect, by.x = "row.id", by.y = "row.id")
  intersect1 <- as.data.frame(intersect1)

  intersect2 <- merge(intersect1, biome1, by.x = "col.id", by.y = "col.id")
  intersect2 <- as.data.frame(intersect2)

  intersect3 <- intersect2 %>%
    dplyr::select(index, B_NAME)

  merged <- merge(occ, intersect3, by = "index", all = TRUE)

  file <- gsub(".csv", "", i)
  file <- gsub("cache/intersect/terrestrial/", "", file)

  fwrite(merged, file = paste0("cache/intersect/biome/", file, "_biome.csv"))

}
