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

