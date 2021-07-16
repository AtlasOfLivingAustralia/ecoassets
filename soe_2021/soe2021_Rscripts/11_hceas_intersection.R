# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(sf)
library(sp)

# Reading shapefiles
hceas <- st_read("cache/IBRA7_HCAS_3zones_dissolve/IBRA7_regions_HCAS_ILZ_3zones_dissolve.shp")
biome <- st_read("cache/ibra7_biomes_dissolve/ibra7_biomes_dissolve.shp")

# Calculating spatial intersection
int <- st_intersection(hceas, biome)
int <- as.data.frame(int)
int$geometry <- NULL
int <- int %>%
  dplyr::select(LandUseZon, B_NAME)
colnames(int) <- c("land_use_zone", "biome")

# Removing duplicates
setkey(int,NULL)
int <- unique(int)

# Reading summarised biome table
biome_merged <- fread("cache/Biome_merged.csv")

# Merging both files to find out which land-use zone is within which biomes
merged <- biome_merged %>% 
  left_join(int, by = "biome")

fwrite(merged, "cache/merged_biome_hceas.csv")
