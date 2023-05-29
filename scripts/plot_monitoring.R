# Generates three plots: one choropleth of the extent of monitoring effort 
# across IBRA/IMCRA regions, and two point maps of the locations of plant and 
# animal monitoring events 
# TODO: revisit colour schemes in plots

source(here("scripts", "plotting_functions.R"))

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)


# shapefiles -------
ibra_sf <- st_read(here("data", "external", "ibra7", "ibra7_regions.shp")) |>
  ms_simplify(keep = 0.2) |> 
  select(ibraRegion = REG_NAME_7) |> 
  st_transform(crs = 4326)

imcra_sf <- st_read(here("data", "external", "imcra4", "imcra4_meso.shp")) |>
  ms_simplify(keep = 0.2) |> 
  select(imcraRegion = MESO_NAME) |> 
  st_transform(crs = 4326)


# monitoring effort ----
ibra_monitoring <- read_csv(here("data", "processed", "monitoring_ibra.csv")) |> 
  group_by(ibraRegion) |> 
  summarise(intensity = n_distinct(year, featureFacet1)/13) |> 
  right_join(ibra_sf, by = join_by(ibraRegion)) |> 
  mutate(intensity = replace_na(intensity, 0)) |> 
  st_as_sf()

imcra_monitoring <- read_csv(here("data", "processed", "monitoring_imcra.csv")) |> 
  group_by(imcraRegion) |> 
  summarise(intensity = n_distinct(year, featureFacet1)/13) |> 
  right_join(imcra_sf, by = join_by(imcraRegion)) |> 
  mutate(intensity = replace_na(intensity, 0)) |> 
  st_as_sf()
  
monitoring_plot <- plot_monitoring_effort(ibra_monitoring, imcra_monitoring)

ggsave(here("plots", "monitoring_effort.png"),
       monitoring_plot,
       height = 10, 
       width = 10, 
       units = "in")


# events point plots -----
events_animals <- read_csv(here("data", "processed", "monitoring.csv")) |> 
  filter(featureFacet1 == "Biological Classification", 
         featureFacet2 == "Animals") |> 
  distinct(decimalLatitude, decimalLongitude, featureFacet2) |>  
  filter(!is.na(decimalLatitude)) |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

animal_events_plot <- plot_sample_event_points(ibra_sf, 
                                               imcra_sf, 
                                               events_animals, 
                                               "#d28700")

ggsave(here("plots", "event_points_animals.png"),
       animal_events_plot,
       height = 10, 
       width = 10, 
       units = "in")

events_plants <- read_csv(here("data", "processed", "monitoring.csv")) |> 
  filter(featureFacet1 == "Biological Classification", 
         featureFacet2 == "Plants") |> 
  distinct(decimalLatitude, decimalLongitude, featureFacet2) |> 
  filter(!is.na(decimalLatitude)) |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

plant_events_plot <- plot_sample_event_points(ibra_sf, 
                                               imcra_sf, 
                                               events_plants,
                                              "#456300")

ggsave(here("plots", "event_points_plants.png"),
       plant_events_plot,
       height = 10, 
       width = 10, 
       units = "in")

