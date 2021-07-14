library(rgdal)
library(raster)
library(sp)
library(sf)
library(tidyverse)
library(data.table)


tern <- fread("cache/data/tern_sep.csv")

tern <- tern %>%
  dplyr::mutate(index = as.integer(rownames(tern)))

occ <- tern %>% 
  dplyr::select(lat, lon, index) %>%
  dplyr::filter(lon < 155,
                lon > 110,
                lat > -45,
                lat < -10)

# Remove records without coordinates
occ <- occ %>%
  dplyr::filter(!is.na(lon)) %>%
  dplyr::filter(!is.na(lat))

# Converting to spatial object
occ.sp <- SpatialPointsDataFrame(occ[,c(2,1)], occ[,-c(2,1)])
occ.sp <- as(occ.sp, "sf")
occ.sp <- st_make_valid(occ.sp)
occ.sp <- st_set_crs(occ.sp, 4326)

# Reading IBRA shape file
ibra <- st_read("IBRA/ibra7_regions.shp")
ibra <- st_transform(ibra, 4326)
ibra <- st_make_valid(ibra)
ibra <- st_collection_extract(ibra, "POLYGON")


# Find which polygon each point is in
intersect <- as.data.frame(st_intersection(occ.sp, ibra))
intersect$geometry <- NULL

intersect1 <- merge(tern, intersect, by = "index")
intersect1 <- as.data.frame(intersect1)

intersect1 <- intersect1 %>% 
  dplyr::select(index, type, REG_NAME_7, lon, lat, key, survey_year)

fwrite(intersect1, "cache/data/tern_intersection_ibra.csv")
