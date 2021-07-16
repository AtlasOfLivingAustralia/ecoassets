# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(tidyverse)

# Reading data file
land_use_zone <- fread("cache/merged_biome_hceas.csv")

# Changing column names
colnames(land_use_zone)[8] <- "sppInPa"
colnames(land_use_zone)[13] <- "sppOutPa"
colnames(land_use_zone)[12] <- "sppInPa_wons"
colnames(land_use_zone)[17] <- "sppOutPa_wons"

# Adding counts to match the land-use zones
land_use_zone <- land_use_zone %>% 
  group_by(land_use_zone, YearRange) %>% 
  summarise(sppByBiome = sum(sppByBiome),
            sppInPa = sum(sppInPa),
            sppOutPa = sum(sppOutPa),
            sppByBiome_epbc = sum(sppByBiome_epbc),
            sppInPa_epbc = sum(sppInPa_epbc),
            sppOutPa_epbc = sum(sppOutPa_epbc),
            sppByBiome_introduced = sum(sppByBiome_introduced),
            sppInPa_introduced = sum(sppInPa_introduced),
            sppOutPa_introduced = sum(sppOutPa_introduced),
            sppByBiome_invasive = sum(sppByBiome_invasive),
            sppInPa_invasive = sum(sppInPa_invasive),
            sppOutPa_invasive = sum(sppOutPa_invasive),
            sppByBiome_wons = sum(sppByBiome_wons),
            sppInPa_wons = sum(sppInPa_wons),
            sppOutPa_wons = sum(sppOutPa_wons))


# Creating subgroups
# All species
overall <- land_use_zone %>%
  dplyr::select(land_use_zone, YearRange, sppByBiome, sppInPa, sppOutPa)
overall <- overall %>%
  mutate(overall = paste0(round(sppByBiome/sum(sppByBiome)*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa/sum(sppByBiome)*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa/sum(sppByBiome)*100, 2), "%"),
         group = "Overall")
overall <- overall %>%
  dplyr::select(land_use_zone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# EPBC listed species
epbc <- land_use_zone %>%
  dplyr::select(land_use_zone, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_epbc, sppInPa_epbc, sppOutPa_epbc)

epbc <- epbc %>%
  mutate(overall = paste0(round(sppByBiome_epbc/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_epbc/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_epbc/sppByBiome*100, 2), "%"),
         group = "EPBC listed species")
epbc <- epbc %>%
  dplyr::select(land_use_zone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# GRIIS (introduced species)
introduced <- land_use_zone %>%
  dplyr::select(land_use_zone, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_introduced, sppInPa_introduced, sppOutPa_introduced)
introduced <- introduced %>%
  mutate(overall = paste0(round(sppByBiome_introduced/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_introduced/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_introduced/sppByBiome*100, 2), "%"),
         group = "Introduced species")
introduced <- introduced %>%
  dplyr::select(land_use_zone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# GRIIS (invasive species)
invasive <- land_use_zone %>%
  dplyr::select(land_use_zone, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_invasive, sppInPa_invasive, sppOutPa_invasive)
invasive <- invasive %>%
  mutate(overall = paste0(round(sppByBiome_invasive/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_invasive/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_invasive/sppByBiome*100, 2), "%"),
         group = "Invasive species")
invasive <- invasive %>%
  dplyr::select(land_use_zone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# Weeds of national significance
wons <- land_use_zone %>%
  dplyr::select(land_use_zone, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_wons, sppInPa_wons, sppOutPa_wons)
wons <- wons %>%
  mutate(overall = paste0(round(sppByBiome_wons/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_wons/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_wons/sppByBiome*100, 2), "%"),
         group = "Weeds of national significance")
wons <- wons %>%
  dplyr::select(land_use_zone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

###############################
df <- rbind(overall, epbc, introduced, invasive, wons)
head(df)

colnames(df) <- c("land_use_zone", "year_range", "group", "overall", "species_in_pa", "species_out_pa")

head(df)

df1 <- df %>%
  tidyr::pivot_longer(
    cols = c("overall", "species_in_pa", "species_out_pa"),
    names_to = "status",
    values_to = "percentages")

df1 <- df1 %>% 
  mutate(status = replace(status, status == "overall", "Overall scenario"),
         status = replace(status, status == "species_in_pa", "Inside protected areas"),
         status = replace(status, status == "species_out_pa", "Outside protected areas"))

df1 <- df1 %>%
  filter(group %in% c("EPBC listed species", "Introduced species", "Invasive species", "Weeds of national significance"))
head(df1)

fwrite(df1, "cache/land_use_zones_summarised_version.csv")

###################################
# Creating plots
df <- fread("cache/land_use_zones_summarised_version.csv")

ggplot(df) +
  geom_line(aes(x = year_range, y = percentages, group = status, colour = status)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("% of species") + facet_grid(cols = vars(group), rows = vars(land_use_zone), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))


df %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # default facetting isn't pretty
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species") +
  facet_grid(cols = vars(land_use_zone), rows = vars(group), scales = "free_y") +
  scale_colour_manual(values = c("green4", "blue", "black"))


df %>%
  dplyr::filter(group == "Introduced species") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # default facetting isn't pretty
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species") +
  facet_grid(cols = vars(land_use_zone), rows = vars(group), scales = "free_y") +
  scale_colour_manual(values = c("green4", "blue", "black"))


df %>%
  dplyr::filter(group == "Invasive species") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # default facetting isn't pretty
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species") +
  facet_grid(cols = vars(land_use_zone), rows = vars(group), scales = "free_y") +
  scale_colour_manual(values = c("green4", "blue", "black"))


df %>%
  dplyr::filter(group == "Weeds of national significance") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # default facetting isn't pretty
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species") +
  facet_grid(cols = vars(land_use_zone), rows = vars(group), scales = "free_y") +
  scale_colour_manual(values = c("green4", "blue", "black"))

