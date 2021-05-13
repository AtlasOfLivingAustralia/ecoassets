# options(java.parameters = "-Xmx6g")

library(data.table)
library(ggplot2)
library(sf)
library(pbapply)
library(parallel)
# library(readr) # NOTE: read_rds is deprecated + only wraps readRDS with no changes


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
capad_albers_buffered <- capad_albers_buffered[,
  c("PA_ID", "NAME", "TYPE", "TYPE_ABBR", "GAZ_DATE", "geometry")] # reduce to only layers of interest

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
    # st_bbox
    # st_cast(bbox, "POLYGON")
    # above could replace below
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

# work out which elements of crop_list are not empty
crop_size <- unlist(lapply(crop_list, nrow))
keep_entries <- which(crop_size > 0)
# saveRDS(keep_entries, "./data/raw_ALA/keep_entries.rds")
# keep_entries <- readRDS("./data/raw_ALA/keep_entries.rds")

## load/save CAPAD sections
crop_list <- crop_list[keep_entries]
# saveRDS(crop_list, "./data/spatial/CAPAD_crop_list.rds")
# crop_list <- readRDS("./data/spatial/CAPAD_crop_list.rds")


# extract CAPAD at points
# subset data using data.table
df <- data_in[, .(index, decimalLongitude, decimalLatitude)]

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

# create a single list where the capad tile and associated points are in a single slot
combined_list <- pblapply(seq_along(crop_list), function(a){
  list(tile = crop_list[[a]], points = data_box_list[[a]])})


## if CAPAD hasn't been subset to a smaller number of columns, do that here
# combined_list <- lapply(combined_list, function(a){
#   x <- a$tile[, c("PA_ID", "NAME", "TYPE", "TYPE_ABBR", "GAZ_DATE", "geometry")]
#   return(list(tile = x, points = a$points))
# })

## load/save
# saveRDS(combined_list, "./data/spatial/combined_list.rds")
# combined_list <- readRDS("./data/spatial/combined_list.rds")

# # plot an example tile:
# a <- combined_list[[1010]]
# ggplot() + geom_sf(
#   aes(color = PA_ID),
#   data = a$tile) +
#   geom_sf(data = a$points, color = "red")

# next - extract CAPAD values at points
cl <- makeCluster(6)
clusterEvalQ(cl, {library(sf)})
point_extraction_list <- pbapply::pblapply(combined_list, function(a){
  intersect_list <- sf::st_intersects(a$tile, a$points, sparse = TRUE)
  intersect_df_list <- lapply(seq_along(intersect_list), function(b){
    if(length(intersect_list[[b]]) > 0){
      data.frame(capad = b, points = intersect_list[[b]])
    }else{NULL}
  })
  intersect_df <- do.call(rbind, intersect_df_list)
  # anyDuplicated(a$points$index) # test - should equal zero
  # extract required information on national parks for each point
  result <- as.data.frame(a$tile)[intersect_df$capad, c("PA_ID", "NAME", "TYPE_ABBR")]
  result$index <- a$points$index[intersect_df$points]
  return(result)
}, cl = cl)
stopCluster(cl)

saveRDS(point_extraction_list, "./data/spatial/point_extraction_list.rds")

# rbind to a single data.table

# merge with data_in
