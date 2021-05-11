# options(java.parameters = "-Xmx6g")

library(data.table)
library(ggplot2)
library(sf)
library(pbapply)
library(parallel)


## OPTIONAL SECTION: If data are only available as cached zip files
# cache_dir <- "./data/raw_ALA"
# cached_files <- list.files(cache_dir)
# cached_files <- cached_files[grepl(".zip$", cached_files)]
# cached_df <- data.frame(
#   source = paste0(cache_dir, "/", cached_files),
#   out = paste0(cache_dir, "/", gsub(".zip", "", cached_files)))
# # below section only needs to be run once, to unzip your files
# # invisible(lapply(
# #   split(cached_df, seq_len(nrow(cached_df))),
# #   function(a){unzip(a$source, exdir = a$out)}))
#
# # detect all files named some variant of 'data.csv'
# all_files <- unlist(lapply(cached_df$out, function(a){
#   all_files <- list.files(a)
#   return(
#     paste0(a, "/", all_files[grepl("^data", all_files)]))
# }))


# Current method: Import rds files and merge them together
data_dir <- "./data/raw_ALA"
files <- paste(data_dir, list.files(data_dir), sep = "/")
data_list <- lapply(files, function(a){readRDS(a)})
data_in <- data.table::setDT(data.table::rbindlist(data_list))
# or if in csv
# data_list <- data.table::rbindlist(
#  lapply(all_files, function(a){data.table::fread(file = a)})) # if csv

# corrections to the dataset
colnames(data_in)[13:15] <- c(
  "australianStatesAndTerritories",
  "iBRA7Regions",
  "CAPAD2016Terrestrial")
index_vector <- seq_len(nrow(data_in))
data_in[, index := index_vector]  # required for later work

## optional saving/loading stage
# saveRDS(data_in, "./data/raw_ALA/data_in.rds")
# data_in <- readRDS("./data/raw_ALA/data_in.rds")


# import the CAPAD layer
capad <- st_read("./data/spatial/CAPAD2020_terrestrial.shp") # st_crs(capad)
capad1 <- capad %>%
  dplyr::mutate(col.id = rownames(capad))
capad1$col.id <- as.integer(capad1$col.id)

capad_albers <- st_transform(capad1, crs = st_crs(3577))
capad_albers_buffered <- st_buffer(capad_albers, dist = 0)
## save/load spatial data
# saveRDS(capad_albers_buffered, "./data/spatial/capad_albers_buffered.rds")
# capad_albers_buffered <- readRDS("./data/spatial/capad_albers_buffered.rds")

# note: uses albers & buffering to avoid intersection errors as per:
# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec



# plot this
# capad_simple <- sf::st_simplify(capad_albers, dTolerance = 10^2)
# map <- ggplot(capad_simple) +
#   geom_sf(fill = "grey90", size = 0.1) +
#   theme_bw()
# #  geom_sf(data = bb_sf, color = "red", fill = NA)
# ggsave("./plots/capad2020_albers.pdf",
#   plot = map,
#   width = 10,
#   height = 8,
#   units = "cm")

# create bounding boxes for 1-degree squares, use to crop the data
box_extents <- expand.grid(
  x_min = seq(110, 154, 1),
  y_min = seq(-45, -9, 1))
box_extents$x_max <- box_extents$x_min + 1
box_extents$y_max <- box_extents$y_min + 1

# extract CAPAD within each degree square
extent_list <- split(box_extents, seq_len(nrow(box_extents)))

# create bounding boxes
box_list <- lapply(
  # cl,
  extent_list,
  function(a){
    bounding_box <- data.frame(
      x = c(a$x_min, a$x_max, a$x_max, a$x_min, a$x_min),
      y = c(a$y_min, a$y_min, a$y_max, a$y_max, a$y_min))
    bb_wkt <- st_as_text(
      st_polygon(list(as.matrix(bounding_box))))
    bb_sf <- st_as_sf(
      data.frame(x = bb_wkt),
      wkt = "x",
      crs = 4283)
    return(st_transform(bb_sf, crs = st_crs(3577)))
})

# then intersect CAPAD within each box via parallel
cl <- makeCluster(6)
clusterExport(cl, "capad_albers_buffered")
clusterEvalQ(cl, {library(sf)})
crop_list <- pbapply::pblapply(
  box_list,
  function(a){
    st_intersection(capad_albers_buffered, a)
  },
  cl = cl)
stopCluster(cl)
# 15:40

## load/save CAPAD sections
# saveRDS(crop_list, "./data/spatial/CAPAD_crop_list.rds")
# crop_list <- readRDS("./data/spatial/CAPAD_crop_list.rds")

# work out which elements of crop_list are not empty
crop_size <- unlist(lapply(crop_list, nrow))
keep_entries <- which(crop_size > 0)
# saveRDS(keep_entries, "./data/raw_ALA/keep_entries.rds")
crop_list <- crop_list[keep_entries]


# extract CAPAD at points
# subset data using data.table
df <- data_in[, .(index, decimalLongitude, decimalLatitude)]

# UP TO HERE

cl <- makeCluster(6)
clusterExport(cl, "df")
clusterEvalQ(cl, {library(sf);library(data.table)})
data_box_list <- pbapply::pblapply(extent_list[keep_entries], function(a){
  points_tr <- df[
    decimalLongitude > a$x_min & decimalLongitude <= a$x_max &
    decimalLatitude > a$y_min & decimalLatitude <= a$y_max&
    !is.na(decimalLongitude) & !is.na(decimalLatitude)]
  # merge with spatial info
  points_sf <- st_as_sf(points_tr,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4283)
  result <- st_transform(points_sf, crs = st_crs(3577))
  return(result)
}, cl = cl)
stopCluster(cl)

## load/save
# saveRDS(data_box_list, "./data/spatial/data_box_list.rds")
# data_box_list <- readRDS("./data/spatial/data_box_list.rds")

# next - extract CAPAD values at points and rbind to a single data.table 






## BELOW OLD

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
