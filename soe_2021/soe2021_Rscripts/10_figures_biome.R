library(tidyverse)
library(patchwork)
library(data.table)

biome <- fread("Biome_merged.csv")
head(biome)

colnames(biome)[8] <- "sppInPa"
colnames(biome)[13] <- "sppOutPa"

overall <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome, sppInPa, sppOutPa)
overall <- overall %>%
  mutate(overall = paste0(round(sppByBiome/sum(sppByBiome)*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa/sum(sppByBiome)*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa/sum(sppByBiome)*100, 2), "%"),
         group = "Overall")

overall <- overall %>%
  dplyr::select(biome, YearRange, group, overall, sppInPa_per, sppOutPa_per)


epbc <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_epbc, sppInPa_epbc, sppOutPa_epbc)

epbc <- epbc %>%
  mutate(overall = paste0(round(sppByBiome_epbc/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_epbc/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_epbc/sppByBiome*100, 2), "%"),
         group = "EPBC listed species")
epbc <- epbc %>%
  dplyr::select(biome, YearRange, group, overall, sppInPa_per, sppOutPa_per)


introduced <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_introduced, sppInPa_introduced, sppOutPa_introduced)

introduced <- introduced %>%
  mutate(overall = paste0(round(sppByBiome_introduced/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_introduced/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_introduced/sppByBiome*100, 2), "%"),
         group = "Introduced species")
introduced <- introduced %>%
  dplyr::select(biome, YearRange, group, overall, sppInPa_per, sppOutPa_per)

invasive <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_invasive, sppInPa_invasive, sppOutPa_invasive)

invasive <- invasive %>%
  mutate(overall = paste0(round(sppByBiome_invasive/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_invasive/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_invasive/sppByBiome*100, 2), "%"),
         group = "invasive species")
invasive <- invasive %>%
  dplyr::select(biome, YearRange, group, overall, sppInPa_per, sppOutPa_per)

wons <- biome %>%
  dplyr::select(biome, YearRange, sppByBiome, sppInPa, sppOutPa, sppByBiome_wons, sppInPa_WoNS, sppOutPa_WoNS)

wons <- wons %>%
  mutate(overall = paste0(round(sppByBiome_wons/sppByBiome*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_WoNS/sppByBiome*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_WoNS/sppByBiome*100, 2), "%"),
         group = "Weeds of national significance")
wons <- wons %>%
  dplyr::select(biome, YearRange, group, overall, sppInPa_per, sppOutPa_per)

###############################
df <- rbind(overall, epbc, introduced, invasive, wons)
head(df)

colnames(df) <- c("biome", "YearRange", "group", "overall", "sppInPa", "sppOutPa")

head(df)

df1 <- df %>%
  tidyr::pivot_longer(
    cols = c("overall", "sppInPa", "sppOutPa"),
    names_to = "status",
    values_to = "percentages")
head(df1)

fwrite(df1, "biome_revised.csv")

df1 <- fread("biome_revised.csv")

ggplot(df1) +
  geom_line(aes(x = YearRange, y = percentages, group = status, colour = status)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("Proportion of species") + facet_grid(cols = vars(group), rows = vars(biome), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))


df1 %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = YearRange, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("Proportion of species") + facet_grid(cols = vars(biome), rows = vars(group), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))

df1 %>%
  dplyr::filter(group == "Introduced species") %>% 
  ggplot( aes(x = YearRange, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("Proportion of species") + facet_grid(cols = vars(biome), rows = vars(group), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))

df1 %>%
  dplyr::filter(group == "Invasive species") %>% 
  ggplot( aes(x = YearRange, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("Proportion of species") + facet_grid(cols = vars(biome), rows = vars(group), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))

df1 %>%
  dplyr::filter(group == "Weeds of national significance") %>% 
  ggplot( aes(x = YearRange, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("Proportion of species") + facet_grid(cols = vars(biome), rows = vars(group), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))

