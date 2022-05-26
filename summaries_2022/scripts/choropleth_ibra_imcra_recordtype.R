
# map of IBRA and IMCRA regions showing the proportion of human observations
# across diferent time periods (1900-2020)

library(here)
library(sf)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(ggplot2)
library(showtext)
library(ggnewscale)
library(rmapshaper)
library(magick)


# get records data --------  
ibra_records <- read_csv(here("summaries_2022",
                             "data",
                             "processed",
                             "ibra_record_type.csv")) 

imcra_records <- read_csv(here("summaries_2022",
                              "data",
                              "processed",
                              "imcra_record_type.csv"))


# get spatial layers ------
ibra_shp <- st_read(here("summaries_2022",
                         "data",
                         "raw",
                         "IBRA7_regions",
                         "ibra7_regions.shp")) |> 
  ms_simplify(keep = 0.2)
  
imcra_shp <- st_read(here("summaries_2022",
                          "data",
                          "raw",
                          "imcra_mesoscale_bioregions",
                          "imcra4_meso.shp")) |> 
  ms_simplify(keep = 0.2)


 # calculate proportions --------
ibra_counts <- ibra_records |> 
  rowwise() |> 
  mutate(proportion = (humanObservationCount / 
                         (humanObservationCount + specimenCount + machineObservationCount))) |>
  mutate(prop_discrete = cut(proportion, 
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                             labels = c(0.2, 0.4, 0.6, 0.8, 1.0), 
                             include.lowest = TRUE)) 

imcra_counts <- imcra_records |> 
  rowwise() |> 
  mutate(proportion = (humanObservationCount / 
                         (humanObservationCount + specimenCount + machineObservationCount))) |>
  mutate(prop_discrete = cut(proportion, 
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                             labels = c(0.2, 0.4, 0.6, 0.8, 1.0), 
                             include.lowest = TRUE)) 
             

# join to shapefiles ------
ibra_join <- ibra_counts |>
  full_join(y = ibra_shp, by = c("ibraRegion" = "REG_NAME_7")) |> 
  select(ibraRegion, yearStart, yearEnd, prop_discrete, geometry) |> 
  st_as_sf()

imcra_join <- imcra_counts |> 
  # typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara")) |> 
  full_join(y = imcra_shp, by = c("imcraRegion" = "MESO_NAME")) |> 
  select(imcraRegion, yearStart, yearEnd, prop_discrete, geometry) |> 
  st_as_sf()


# pre-plotting -----
# convert df to lists for using purrr
ibra_join <- ibra_join |> 
  group_by(yearStart) |> 
  group_split(yearStart) |> 
  setNames(unique(ibra_join$yearStart))

imcra_join <- imcra_join |> 
  group_by(yearStart) |> 
  group_split(yearStart) |> 
  setNames(unique(imcra_join$yearStart))


# plotting function
# set up a manual scale to make sure legends are consistent across plots
# drop = FALSE in scale_fill_manual stops it from crashing when the data doesn't have 5 levels 
plotting_fun <- function(imcra_dat, ibra_dat) {
  
  start_year <- unique(imcra_dat$yearStart)
  end_year <- unique(imcra_dat$yearEnd)
  
  ggplot() +
    geom_sf(data = imcra_dat,
            aes(fill = prop_discrete),
            colour = NA) +
    scale_fill_manual(name = "IMCRA",
                      drop = FALSE,
                      labels = c("0.2", "0.4", "0.6", "0.8"),
                      # 5-class BuPu
                      values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c"),
                      guide = guide_colorsteps(
                        direction = "horizontal",
                        label.position = "bottom",
                        title.position = "left",
                        title.vjust = 0.8, 
                        title.hjust = 0.8)) +
    new_scale_fill() +
    geom_sf(data = ibra_dat,
            aes(fill = prop_discrete),
            colour = NA) +
    scale_fill_manual(name = "IBRA",
                      drop = FALSE,
                      labels = c("0.2", "0.4", "0.6", "0.8"),
                      # 5-class YlOrBr
                      values = c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
                      guide = guide_colorsteps(
                        direction = "horizontal",
                        label.position = "bottom",
                        title.position = "left",
                        title.vjust = 0.8, 
                        title.hjust = 0.8)) +
    annotate("text", 
             x = 133,
             y = -45.5,
             label = str_glue("Proportion of human observations: {start_year} - {end_year}"),
             size = 8) +
    coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
    theme_void() +
    theme(text = element_text(family = "lato"),
          title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.key.width = unit(12, 'mm'),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white')) 
}

# plot (static) -------

# things to make it look nice
font_add_google("Lato", "lato")
showtext_auto()

# generate plots
all_plots <- map2(imcra_join, ibra_join, plotting_fun)

# set the names of plots to save
plotnames <- map(names(all_plots), ~paste0("summaries_2022/plots/ibra_imcra_", ., ".png")) 

# save plots
walk2(plotnames, all_plots, ~ggsave(filename = .x, plot = .y, 
                                             height = 10, width = 10, units = "in"))
     
# animate! ------

list.files(path = "./summaries_2022/plots", pattern = "*.png", full.names = TRUE) |> 
  map(image_read) |>  # reads each path file
  image_join() |>  # joins image
  image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
  image_write("./summaries_2022/plots/ibra_imcra_observations.gif")

# save as tiff for high def
# ggsave(here(
#   "projects",
#   "plant-conservation-conf",
#   "plots",
#   "choropleth-ibra-imcra_reversed.tiff"),
#   p, height = 10, width = 10, units = "in", device='tiff', dpi = 300)

