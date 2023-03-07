# visualisations of biodiversity data facets: heatmaps, choropleths

library(here)
library(vroom)
library(dplyr)
library(stringr)
library(purrr)
library(sf)
library(ggplot2)
library(ggnewscale)
library(rmapshaper)
library(magick)
library(showtext)

source(here("scripts", "plotting_functions.R"))

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)

# heatmaps -----
# invasive and threatened species for IBRA and IMCRA regions


# 01. IBRA GRIIS -------
ibra_griis <- vroom(here("data", "processed", "ibra_griis_5year.csv"))

# get proportions of invasive/introduced species
ibra_invasive <- ibra_griis |> 
  as_period_string() |> 
  mutate(status = case_when(
    griisStatus == "Native" ~ "Native",
    TRUE ~ "Introduced / Invasive")) |> 
  select(ibraRegion, period, status, speciesCount) |> 
  group_by(ibraRegion, period, status) |> 
  mutate(speciesCount = sum(speciesCount)) |>
  ungroup() |> 
  distinct() |> 
  group_by(ibraRegion, period) |> 
  mutate(totalCount = sum(speciesCount)) |> 
  ungroup() |> 
  filter(status == "Introduced / Invasive") |> 
  rowwise() |> 
  mutate(prop_invasive = speciesCount / totalCount) |> 
  ungroup()

# check distribution
hist(ibra_invasive$prop_invasive)         # skewed
hist(log(ibra_invasive$prop_invasive))    # normal(ish)
hist(ibra_invasive$speciesCount)
hist(log(ibra_invasive$speciesCount))

# proportional values
p <- plot_heatmap(plot = ggplot(data = ibra_invasive, 
                                aes(x = period, 
                                    y = reorder(ibraRegion, desc(ibraRegion)),
                                    fill = prop_invasive)),
                  legend_title = "Proportion of introduced/invasive\nspecies in IBRA regions",
                  pal = "YlOrBr",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_ibra_griis_prop.png"),
       p, height = 16, width = 8, units = "in")


# absolute values
p <- plot_heatmap(plot = ggplot(data = ibra_invasive,
                                aes(x = period,
                                    y = reorder(ibraRegion, desc(ibraRegion)),
                                    fill = speciesCount)),
                  legend_title = "Number of introduced/invasive\nspecies in IBRA regions", 
                  pal = "YlOrBr",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_ibra_griis_abs.png"),
       p, height = 16, width = 8, units = "in")


# 02. IBRA EPBC -------
ibra_epbc <- vroom(here("data", "processed", "ibra_epbc_5year.csv"))

# get proportions of threatened species
ibra_listed <- ibra_epbc |> 
  as_period_string() |> 
  mutate(status = case_when(
    epbcStatus == "Not listed" ~ "Not listed",
    TRUE ~ "listed")) |>  
  select(ibraRegion, period, status, speciesCount) |> 
  group_by(ibraRegion, period, status) |> 
  mutate(speciesCount = sum(speciesCount)) |>
  ungroup() |> 
  distinct() |> 
  group_by(ibraRegion, period) |> 
  mutate(totalCount = sum(speciesCount)) |> 
  ungroup() |> 
  filter(status == "listed") |> 
  rowwise() |> 
  mutate(prop_listed = speciesCount / totalCount) |> 
  ungroup()

# check distribution
hist(ibra_listed$prop_listed)       
hist(log(ibra_listed$prop_listed))  
hist(ibra_listed$speciesCount)       
hist(log(ibra_listed$speciesCount))

# proportional values
p <- plot_heatmap(plot = ggplot(data = ibra_listed,
                                aes(x = period,
                                    y = reorder(ibraRegion, desc(ibraRegion)),
                                    fill = prop_listed)),
                  legend_title = "Proportion of EPBC-listed\nspecies in IBRA regions",
                  pal = "YlOrBr",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_ibra_epbc_prop.png"),
       p, height = 16, width = 8, units = "in")

# absolute values
p <- plot_heatmap(plot = ggplot(data = ibra_listed,
                                aes(x = period,
                                    y = reorder(ibraRegion, desc(ibraRegion)),
                                    fill = speciesCount)),
                  legend_title = "Number of EPBC-listed\nspecies in IBRA regions", 
                  pal = "YlOrBr",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_ibra_epbc_abs.png"),
       p, height = 16, width = 8, units = "in")


# 03. IMCRA GRIIS -------
imcra_griis <- vroom(here("data", "processed", "imcra_griis_5year.csv"))

# get proportions of invasive/introduced species
imcra_invasive <- imcra_griis |> 
  as_period_string() |> 
  mutate(status = case_when(
    griisStatus == "Native" ~ "Native",
    TRUE ~ "Introduced / Invasive")) |> 
  select(imcraRegion, period, status, speciesCount) |> 
  group_by(imcraRegion, period, status) |> 
  mutate(speciesCount = sum(speciesCount)) |>
  ungroup() |> 
  distinct() |> 
  group_by(imcraRegion, period) |> 
  mutate(totalCount = sum(speciesCount)) |> 
  ungroup() |> 
  filter(status == "Introduced / Invasive") |> 
  rowwise() |> 
  mutate(prop_invasive = speciesCount / totalCount) |> 
  ungroup() |> 
  # typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara"))

# check distribution
hist(imcra_invasive$prop_invasive)
hist(log(imcra_invasive$prop_invasive))
hist(imcra_invasive$speciesCount)
hist(log(imcra_invasive$speciesCount))

# proportional values
p <- plot_heatmap(plot = ggplot(data = imcra_invasive,
                                aes(x = period,
                                    y = reorder(imcraRegion, desc(imcraRegion)),
                                    fill = prop_invasive)),
                  legend_title = "Proportion of introduced/invasive\nspecies in IMCRA regions",
                  pal = "BuPu",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_imcra_griis_prop.png"),
       p, height = 16, width = 8, units = "in")

# absolute values
p <- plot_heatmap(plot = ggplot(data = imcra_invasive,
                                aes(x = period,
                                    y = reorder(imcraRegion, desc(imcraRegion)),
                                    fill = speciesCount)),
                  legend_title = "Number of introduced/invasive\nspecies in IMCRA regions", 
                  pal = "BuPu",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_imcra_griis_abs.png"),
       p, height = 16, width = 8, units = "in")


# 04. IMCRA EPBC -------
imcra_epbc <- vroom(here("data", "processed", "imcra_epbc_5year.csv"))

# get proportions of threatened species
imcra_listed <- imcra_epbc |> 
  as_period_string() |> 
  mutate(status = case_when(
    epbcStatus == "Not listed" ~ "Not listed",
    TRUE ~ "listed")) |>  
  select(imcraRegion, period, status, speciesCount) |> 
  group_by(imcraRegion, period, status) |> 
  mutate(speciesCount = sum(speciesCount)) |>
  ungroup() |> 
  distinct() |> 
  group_by(imcraRegion, period) |> 
  mutate(totalCount = sum(speciesCount)) |> 
  ungroup() |> 
  filter(status == "listed") |> 
  rowwise() |> 
  mutate(prop_listed = speciesCount / totalCount) |>
  ungroup() |> 
  # typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara"))

# check distribution
hist(imcra_listed$prop_listed)       
hist(log(imcra_listed$prop_listed))  
hist(imcra_listed$speciesCount)       
hist(log(imcra_listed$speciesCount))

# proportional values
p <- plot_heatmap(plot = ggplot(data = imcra_listed,
                                aes(x = period,
                                    y = reorder(imcraRegion, desc(imcraRegion)),
                                    fill = prop_listed)),
                  legend_title = "Proportion of EPBC-listed\nspecies in IMCRA regions",
                  pal = "BuPu",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_imcra_epbc_prop.png"),
       p, height = 16, width = 8, units = "in")

# absolute values
p <- plot_heatmap(plot = ggplot(data = imcra_listed,
                                aes(x = period,
                                    y = reorder(imcraRegion, desc(imcraRegion)),
                                    fill = speciesCount)),
                  legend_title = "Number of EPBC-listed\nspecies in IMCRA regions", 
                  pal = "BuPu",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_imcra_epbc_abs.png"),
       p, height = 16, width = 8, units = "in")


# choropleths -----

# records and spatial layers
ibra_records <- vroom(here("data", "processed", "ibra_record_type.csv")) 
ibra_shp <- st_read(here("data", "external", "ibra7", "ibra7_regions.shp")) |>
  ms_simplify(keep = 0.2)

imcra_records <- vroom(here("data", "processed", "imcra_record_type.csv"))
imcra_shp <- st_read(here("data", "external", "imcra4", "imcra4_meso.shp")) |>
  ms_simplify(keep = 0.2)

# calculate proportions
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

# join to spatial layers and convert to lists for using purrr
ibra_join <- ibra_counts |>
  full_join(y = ibra_shp, by = c("ibraRegion" = "REG_NAME_7")) |> 
  select(ibraRegion, yearStart, yearEnd, prop_discrete, geometry) |> 
  st_as_sf() |> 
  group_by(yearStart) |> 
  group_split(yearStart) %>%  # base pipes still unhappy about . as placeholder          
  set_names(map(., ~.x$yearStart[1]))

imcra_join <- imcra_counts |> 
  # typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara")) |> 
  full_join(y = imcra_shp, by = c("imcraRegion" = "MESO_NAME")) |> 
  select(imcraRegion, yearStart, yearEnd, prop_discrete, geometry) |> 
  st_as_sf() |> 
  group_by(yearStart) |> 
  group_split(yearStart) %>% 
  set_names(map(., ~.x$yearStart[1]))


# static plots -----

# generate plots
all_plots <- map2(imcra_join, ibra_join, plot_choropleth)

# set names of plots to save
plotnames <- map(names(all_plots), ~paste0("plots/choropleth_ibra_imcra_", ., ".png")) 

# save plots
walk2(plotnames, all_plots, ~ggsave(filename = .x, 
                                    plot = .y, 
                                    height = 10, 
                                    width = 10, 
                                    units = "in"))

# animate! ------
list.files(path = "./plots", pattern = "^choropleth.*png$", full.names = TRUE) |> 
  map(image_read) |>  # reads each path file
  image_join() |>  # joins image
  image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
  image_write("./plots/choropleth_ibra_imcra_observations.gif")
