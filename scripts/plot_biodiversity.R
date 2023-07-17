# Generates eight heatmaps, eleven choropleths, and one gif 
# heatmaps show absolute and proportional values of introduced and threatened 
# species in IBRA and IMCRA bioregions
# choropleths and gif show the proportion of human observations in each 
# bioregion across aggregated time periods 

source(here("scripts", "plotting_functions.R"))

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)


# heatmaps ------

get_proportions <- function(df, region_type, status_val) {
  
  df |> 
    group_by({{region_type}}, yearStart, yearEnd, status) |>
    mutate(groupedSpeciesCount = sum(speciesCount)) |>
    ungroup() |> 
    distinct({{region_type}}, yearStart, yearEnd, status, groupedSpeciesCount) |> 
    group_by({{region_type}}, yearStart, yearEnd) |>
    mutate(totalCount = sum(groupedSpeciesCount)) |>
    ungroup() |> 
    filter(status == status_val) |>
    rowwise() |>
    mutate(prop = groupedSpeciesCount / totalCount) |>
    ungroup() |> 
    mutate(period = paste0(yearStart, "—", yearEnd))
}


### 01. ibra griis -----
ibra_griis <- read_csv(here("data", 
                            "summary_introduced_spp_occ_terrestrial",
                            "summary_introduced_spp_occ_terrestrial.csv"))

ibra_invasive <- ibra_griis |> 
  mutate(status = case_when(griisStatus == "Native" ~ "Native",
                            TRUE ~ "Introduced / Invasive")) |> 
  get_proportions(ibraRegion, "Introduced / Invasive")

# proportional values 
plot_heatmap(plot = ggplot(data = ibra_invasive, 
                           aes(x = period, 
                               y = reorder(ibraRegion, desc(ibraRegion)),
                               fill = prop)),
             legend_title = "Proportion of introduced/invasive\nspecies in IBRA regions",
             pal = "YlOrBr",
             n_breaks = 5)

ggsave(here("plots", "heatmap_ibra_griis_prop.png"),
       height = 16, width = 8, units = "in")

# absolute values
plot_heatmap(plot = ggplot(data = ibra_invasive,
                           aes(x = period,
                               y = reorder(ibraRegion, desc(ibraRegion)),
                               fill = groupedSpeciesCount)),
             legend_title = "Number of introduced/invasive\nspecies in IBRA regions", 
             pal = "YlOrBr",
             n_breaks = 6)

ggsave(here("plots", "heatmap_ibra_griis_abs.png"),
       height = 16, width = 8, units = "in")


### 02. imcra griis -----
imcra_griis <- read_csv(here("data",
                             "summary_introduced_spp_occ_marine",
                             "summary_introduced_spp_occ_marine.csv"))

imcra_invasive <- imcra_griis |> 
  mutate(status = case_when(griisStatus == "Native" ~ "Native",
                            TRUE ~ "Introduced / Invasive")) |> 
  get_proportions(imcraRegion, "Introduced / Invasive") |> 
  # typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara"))

# proportional values
plot_heatmap(plot = ggplot(data = imcra_invasive,
                           aes(x = period,
                               y = reorder(imcraRegion, desc(imcraRegion)),
                               fill = prop)), 
                  legend_title = "Proportion of introduced/invasive\nspecies in IMCRA regions",
                  pal = "BuPu",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_imcra_griis_prop.png"),
       height = 16, width = 8, units = "in")

# absolute values
plot_heatmap(plot = ggplot(data = imcra_invasive,
                           aes(x = period,
                               y = reorder(imcraRegion, desc(imcraRegion)),
                               fill = groupedSpeciesCount)), 
                  legend_title = "Number of introduced/invasive\nspecies in IMCRA regions", 
                  pal = "BuPu",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_imcra_griis_abs.png"),
       height = 16, width = 8, units = "in")


### 03. ibra epbc ------
ibra_epbc <- read_csv(here("data",
                           "summary_threatened_spp_occ_terrestrial",
                           "summary_threatened_spp_occ_terrestrial.csv"))

ibra_listed <- ibra_epbc |> 
  mutate(status = case_when(epbcStatus == "Not listed" ~ "Not listed",
                            TRUE ~ "listed")) |> 
  get_proportions(ibraRegion, "listed")

# proportional values
plot_heatmap(plot = ggplot(data = ibra_listed,
                           aes(x = period,
                               y = reorder(ibraRegion, desc(ibraRegion)),
                               fill = prop)),
                  legend_title = "Proportion of EPBC-listed\nspecies in IBRA regions",
                  pal = "YlOrBr",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_ibra_epbc_prop.png"),
       height = 16, width = 8, units = "in")

# absolute values
plot_heatmap(plot = ggplot(data = ibra_listed,
                           aes(x = period,
                               y = reorder(ibraRegion, desc(ibraRegion)),
                               fill = groupedSpeciesCount)),
                  legend_title = "Number of EPBC-listed\nspecies in IBRA regions", 
                  pal = "YlOrBr",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_ibra_epbc_abs.png"),
       height = 16, width = 8, units = "in")


### 04. imcra epbc ------
imcra_epbc <- read_csv(here("data",
                            "summary_threatened_spp_occ_marine",
                            "summary_threatened_spp_occ_marine.csv"))

imcra_listed <- imcra_epbc |> 
  mutate(status = case_when(epbcStatus == "Not listed" ~ "Not listed",
                            TRUE ~ "listed")) |> 
  get_proportions(imcraRegion, "listed") |> 
  # typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara"))

# proportional values
plot_heatmap(plot = ggplot(data = imcra_listed,
                           aes(x = period,
                               y = reorder(imcraRegion, desc(imcraRegion)),
                               fill = prop)),
                  legend_title = "Proportion of EPBC-listed\nspecies in IMCRA regions",
                  pal = "BuPu",
                  n_breaks = 5)

ggsave(here("plots", "heatmap_imcra_epbc_prop.png"),
       height = 16, width = 8, units = "in")

# absolute values
plot_heatmap(plot = ggplot(data = imcra_listed,
                           aes(x = period,
                               y = reorder(imcraRegion, desc(imcraRegion)),
                               fill = groupedSpeciesCount)),
                  legend_title = "Number of EPBC-listed\nspecies in IMCRA regions", 
                  pal = "BuPu",
                  n_breaks = 6)

ggsave(here("plots", "heatmap_imcra_epbc_abs.png"),
       height = 16, width = 8, units = "in")


# choropleths -------

### summarise proportions of different record types across time periods ----

loc <- open_dataset("data/interim/rel_distinct_loc.parquet")
occ <- open_dataset("data/interim/rel_occ_counts.parquet") 
                    
year_lookup <- tibble(year = as.double(1900:2020),
                      yearStart = c(rep(1900, times = 71),
                                     rep(seq(1971, 2016, by = 5), each = 5)),
                      yearEnd = c(rep(1970, times = 71),
                                   rep(seq(1975, 2020, by = 5), each = 5)))

record_type_lookup <- tibble(basisOfRecord = c("PRESERVED_SPECIMEN", 
                                               "HUMAN_OBSERVATION",
                                               "OCCURRENCE",
                                               "OBSERVATION",
                                               "MATERIAL_CITATION", 
                                               "MATERIAL_SAMPLE", 
                                               "MACHINE_OBSERVATION",
                                               "LIVING_SPECIMEN"),
                             recordType = c("specimen",
                                            "human_observation",
                                            "unknown",
                                            "human_observation",
                                            "specimen",
                                            "specimen",
                                            "machine_observation",
                                            "specimen"))


joined_lookups <- occ |> 
  filter(year <= 2020) |> 
  left_join(loc, by = join_by(locationID)) |> 
  left_join(record_type_lookup, by = join_by(basisOfRecord)) |> 
  mutate(year = as.numeric(year)) |> 
  left_join(year_lookup, by = join_by(year)) |> 
  compute()

ibra_prop <- joined_lookups |> 
  filter(!is.na(ibraRegion)) |> 
  group_by(ibraRegion, yearStart, yearEnd, recordType) |> 
  summarise(summedCounts = sum(counts), .groups = "drop") |> 
  collect() |> 
  pivot_wider(names_from = recordType, 
              values_from = summedCounts, 
              values_fill = 0) |> 
  rowwise() |> 
  mutate(prop_human_obs = human_observation / 
           sum(specimen, human_observation, unknown, machine_observation))

imcra_prop <- joined_lookups |> 
  filter(!is.na(imcraRegion)) |> 
  group_by(imcraRegion, yearStart, yearEnd, recordType) |> 
  summarise(summedCounts = sum(counts), .groups = "drop") |> 
  collect() |> 
  pivot_wider(names_from = recordType, 
              values_from = summedCounts, 
              values_fill = 0) |> 
  rowwise() |> 
  mutate(prop_human_obs = human_observation / 
           sum(specimen, human_observation, unknown, machine_observation))

# add spatial data
ibra_shp <- st_read(here("data", "external", "ibra7", "ibra7_regions.shp")) |>
  ms_simplify(keep = 0.2)

ibra_sp <- ibra_prop |> 
  rowwise() |> 
  mutate(prop_discrete = cut(prop_human_obs, 
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                             labels = c(0.2, 0.4, 0.6, 0.8, 1.0), 
                             include.lowest = TRUE)) |> 
  full_join(ibra_shp, by = join_by(ibraRegion == REG_NAME_7)) |> 
  select(ibraRegion, yearStart, yearEnd, prop_discrete, geometry) |> 
  st_as_sf() |> 
  group_by(yearStart) |> 
  # base pipes still unhappy about . as placeholder
  group_split(yearStart) %>%           
  set_names(map(., ~.x$yearStart[1]))

imcra_shp <- st_read(here("data", "external", "imcra4", "imcra4_meso.shp")) |>
  ms_simplify(keep = 0.2)

imcra_sp <- imcra_prop |> 
  rowwise() |> 
  mutate(prop_discrete = cut(prop_human_obs, 
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), 
                             labels = c(0.2, 0.4, 0.6, 0.8, 1.0), 
                             include.lowest = TRUE)) |> 
  # fix typo in region name
  mutate(imcraRegion = str_replace(imcraRegion, "Pilbarra", "Pilbara")) |> 
  full_join(imcra_shp, by = join_by(imcraRegion == MESO_NAME)) |> 
  select(imcraRegion, yearStart, yearEnd, prop_discrete, geometry) |> 
  st_as_sf() |> 
  group_by(yearStart) |> 
  group_split(yearStart) %>%            
  set_names(map(., ~.x$yearStart[1]))

# static plots
all_plots <- map2(imcra_sp, ibra_sp, plot_choropleth)

plotnames <- map(names(all_plots), ~paste0("plots/choropleth_", ., ".png")) 

walk2(plotnames, all_plots, ~ggsave(filename = .x, 
                                    plot = .y, 
                                    height = 10, 
                                    width = 10, 
                                    units = "in"))

# animation
list.files(path = "./plots", pattern = "^choropleth.*png$", full.names = TRUE) |> 
  map(image_read) |>  # reads each path file
  image_join() |>  # joins image
  image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
  image_write("./plots/choropleth_human_obs.gif")
