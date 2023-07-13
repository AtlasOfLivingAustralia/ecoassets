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

### 01. ibra griis -----
ibra_griis <- read_csv(here("data", 
                            "summary_introduced_spp_occ_terrestrial",
                            "summary_introduced_spp_occ_terrestrial.csv"))

ibra_invasive <- ibra_griis |>
  mutate(status = case_when(griisStatus == "Native" ~ "Native",
                            TRUE ~ "Introduced / Invasive")) |> 
  group_by(ibraRegion, yearStart, yearEnd, status) |>
  mutate(groupedSpeciesCount = sum(speciesCount)) |>
  ungroup() |> 
  distinct(ibraRegion, yearStart, yearEnd, status,groupedSpeciesCount) |> 
  group_by(ibraRegion, yearStart, yearEnd) |>
  mutate(totalCount = sum(groupedSpeciesCount)) |>
  ungroup() |> 
  filter(status == "Introduced / Invasive") |>
  rowwise() |>
  mutate(prop_invasive = groupedSpeciesCount / totalCount) |>
  ungroup() |> 
  mutate(period = paste0(yearStart, "—", yearEnd))

# proportional values 
plot_heatmap(plot = ggplot(data = ibra_invasive, 
                           aes(x = period, 
                               y = reorder(ibraRegion, desc(ibraRegion)),
                               fill = prop_invasive)),
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



### 03. ibra epbc ------



### 04. imcra epbc ------



# choropleths -------
