library(rgdal)
library(raster)
library(sp)
library(sf)
library(tidyverse)
library(data.table)

imos_revised <- fread("cache/data/imos_surveyYear_sep.csv")

# Subsetting required columns
imos_revised <- imos_revised %>% 
  dplyr::select(identifier, westBoundLongitude, eastBoundLongitude, northBoundLatitude, southBoundLatitude, keyword, surveyYear)

# Subset by specific keywords
df1 <- imos_revised[imos_revised$keyword %like% "Oceans", ]
df2 <- imos_revised[imos_revised$keyword %like% "Passive Acoustic ", ]

mergedData <- rbind(df1, df2)

# Removing unnecessary keywords
mergedData1 <- mergedData %>% 
  filter(!str_detect(keyword, 'Global / Oceans'))

# Removing space
mergedData1$keyword <- gsub (" ", "", mergedData1$keyword)

mergedData1 <- mergedData1 %>%
  dplyr::mutate(row = rownames(mergedData1))


# Reading IMCRA shape file
imcra <- st_read("IMCRA/imcra4_meso.shp")
imcra <- st_transform(imcra, 4326)
imcra <- st_make_valid(imcra)
imcra <- st_collection_extract(imcra, "POLYGON")

out <- NULL

for (i in 1:nrow(mergedData1)) try({
  print(i)
  
  # subsetting coordinate columns
  t <- mergedData1 %>%
    dplyr::filter(row == i)
  
  
  t1 <- t[,2:5]
  
  polygon_list <- apply(t1, 1, function(x){
    ## Loop through rows to create extent objects
    shp <- extent(x[c(1, 2, 4, 3)]) ## x[c(2, 1, 4, 3)] to reorder the row to match the order required by raster::extent function. The order should xmin, xmax, ymin, ymax
    ## convert the extent objects to spatial polygons
    shp <- as(shp, 'SpatialPolygons')
    ## convert the spatial polygons to spatial polygons dataframe
    shp <- as(shp, 'SpatialPolygonsDataFrame')
    ## set CRS 
    crs(shp) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
    shp
  })
  
  polygons_list = do.call(rbind, polygon_list)
  
  shp <- st_as_sf(polygons_list)
  
  int <- st_intersection(imcra, shp)
  
  merged <- merge(int, t)
  
  out[[i]] <- merged
  
}, silent = FALSE)

out_df <- do.call(rbind, out)

imos_intersection_imcra$geometry <- NULL
imos_intersection_imcra <- as.data.frame(unlist(imos_intersection_imcra))

fwrite(imos_intersection_imcra, "cache/data/imos_intersection_imcra.csv")

