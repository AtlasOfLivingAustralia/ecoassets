library(tidyverse)

# Reading merged biome data
biome <- fread("cache/sumTable/biome_merged.csv")

# Subset data frames for the calculation purpose
overall <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome)
overall <- overall %>%
  group_by(biome, YearRange) %>%
  mutate(csum=cumsum(sppByBiome),
         group = "Overall scenario")
overall <- overall %>%
  dplyr::select(biome, YearRange, csum, group)

###################
epbc <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome_epbc)
epbc <- epbc %>%
  group_by(biome, YearRange) %>%
  mutate(csum = cumsum(sppByBiome_epbc),
         group = "EPBC listed species")
epbc <- epbc %>%
  dplyr::select(biome, YearRange, csum, group)

###################
introduced <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome_introduced)
introduced <- introduced %>%
  group_by(biome, YearRange) %>%
  mutate(csum = cumsum(sppByBiome_introduced),
         group = "Introduced species")
introduced <- introduced %>%
  dplyr::select(biome, YearRange, csum, group)

###################
invasive <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome_invasive)
invasive <- invasive %>%
  group_by(biome, YearRange) %>%
  mutate(csum = cumsum(sppByBiome_invasive),
         group = "Invasive species")
invasive <- invasive %>%
  dplyr::select(biome, YearRange, csum, group)

###################
wons <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome_wons)
wons <- wons %>%
  group_by(biome, YearRange) %>%
  mutate(csum = cumsum(sppByBiome_wons),
         group = "Weeds of national significance")
wons <- wons %>%
  dplyr::select(biome, YearRange,  csum, group)

df <- rbind(epbc, introduced, invasive, wons)

write.csv(df, "cache/cumulativeValues_biome.csv")

##############################################
# Sample figures
ggplot(df, aes(YearRange, csum, fill = group)) +
  geom_bar(stat = "identity") + facet_wrap(~group)

# Bar chart
df %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = YearRange, y = csum, group = biome)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Number of species", title = "EPBC listed species") + facet_wrap(~biome, nrow = 1, labeller = label_wrap_gen(width=10))

# Heat map
df %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = YearRange, y = biome, fill = csum)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Number of species", title = "EPBC listed species") + 
  scale_fill_viridis_c()
