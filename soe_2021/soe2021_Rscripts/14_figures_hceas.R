# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(data.table)
library(dplyr)
library(tidyverse)

# Reading data file
landUseZone <- fread("cache/sumTable/landUseZone_merged.csv")

# Changing column names
colnames(landUseZone)[8] <- "sppInPa"
colnames(landUseZone)[13] <- "sppOutPa"
colnames(landUseZone)[12] <- "sppInPa_wons"
colnames(landUseZone)[17] <- "sppOutPa_wons"

# Adding counts to match the land-use zones
landUseZone <- landUseZone %>% 
  group_by(landUseZone, YearRange) %>% 
  summarise(sppBylandUseZone = sum(sppBylandUseZone),
            sppInPa = sum(sppInPa),
            sppOutPa = sum(sppOutPa),
            sppBylandUseZone_epbc = sum(sppBylandUseZone_epbc),
            sppInPa_epbc = sum(sppInPa_epbc),
            sppOutPa_epbc = sum(sppOutPa_epbc),
            sppBylandUseZone_introduced = sum(sppBylandUseZone_introduced),
            sppInPa_introduced = sum(sppInPa_introduced),
            sppOutPa_introduced = sum(sppOutPa_introduced),
            sppBylandUseZone_invasive = sum(sppBylandUseZone_invasive),
            sppInPa_invasive = sum(sppInPa_invasive),
            sppOutPa_invasive = sum(sppOutPa_invasive),
            sppBylandUseZone_wons = sum(sppBylandUseZone_wons),
            sppInPa_wons = sum(sppInPa_wons),
            sppOutPa_wons = sum(sppOutPa_wons))


# Creating subgroups
# All species
overall <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone, sppInPa, sppOutPa)
overall <- overall %>%
  mutate(overall = paste0(round(sppBylandUseZone/sum(sppBylandUseZone)*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa/sum(sppBylandUseZone)*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa/sum(sppBylandUseZone)*100, 2), "%"),
         group = "Overall")
overall <- overall %>%
  dplyr::select(landUseZone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# EPBC listed species
epbc <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone, sppInPa, sppOutPa, sppBylandUseZone_epbc, sppInPa_epbc, sppOutPa_epbc)

epbc <- epbc %>%
  mutate(overall = paste0(round(sppBylandUseZone_epbc/sppBylandUseZone*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_epbc/sppBylandUseZone*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_epbc/sppBylandUseZone*100, 2), "%"),
         group = "EPBC listed species")
epbc <- epbc %>%
  dplyr::select(landUseZone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# GRIIS (introduced species)
introduced <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone, sppInPa, sppOutPa, sppBylandUseZone_introduced, sppInPa_introduced, sppOutPa_introduced)
introduced <- introduced %>%
  mutate(overall = paste0(round(sppBylandUseZone_introduced/sppBylandUseZone*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_introduced/sppBylandUseZone*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_introduced/sppBylandUseZone*100, 2), "%"),
         group = "Introduced species")
introduced <- introduced %>%
  dplyr::select(landUseZone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# GRIIS (invasive species)
invasive <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone, sppInPa, sppOutPa, sppBylandUseZone_invasive, sppInPa_invasive, sppOutPa_invasive)
invasive <- invasive %>%
  mutate(overall = paste0(round(sppBylandUseZone_invasive/sppBylandUseZone*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_invasive/sppBylandUseZone*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_invasive/sppBylandUseZone*100, 2), "%"),
         group = "Invasive species")
invasive <- invasive %>%
  dplyr::select(landUseZone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

# Weeds of national significance
wons <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone, sppInPa, sppOutPa, sppBylandUseZone_wons, sppInPa_wons, sppOutPa_wons)
wons <- wons %>%
  mutate(overall = paste0(round(sppBylandUseZone_wons/sppBylandUseZone*100, 2), "%"), 
         sppInPa_per = paste0(round(sppInPa_wons/sppBylandUseZone*100, 2), "%"), 
         sppOutPa_per = paste0(round(sppOutPa_wons/sppBylandUseZone*100, 2), "%"),
         group = "Weeds of national significance")
wons <- wons %>%
  dplyr::select(landUseZone, YearRange, group, overall, sppInPa_per, sppOutPa_per)

###############################
df <- rbind(overall, epbc, introduced, invasive, wons)
colnames(df) <- c("landUseZone", "year_range", "group", "overall", "species_in_pa", "species_out_pa")
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

fwrite(df1, "cache/sumTable/landUseZones_summarised_version.csv")

###################################
# Creating plots
df1 <- fread("cache/sumTable/landUseZones_summarised_version.csv")

# Removing 'overall' values from the group column
df1 <- subset(df1, (group != "Overall"))

# Creating plots
ggplot(df1) +
  geom_line(aes(x = year_range, y = percentages, group = status, colour = status)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("Proportion of species") + facet_grid(cols = vars(group), rows = vars(landUseZone), scales = "free_y", labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black"))


df1 %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species", title = "EPBC listed species") + facet_wrap(~landUseZone, nrow = 1, labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black")) + ggsave("cache/figures/epbc_landUseZone.png")

df1 %>%
  dplyr::filter(group == "Introduced species") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species", title = "Introduced species") + facet_wrap(~landUseZone, nrow = 1, labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black")) + ggsave("cache/figures/introduced_landUseZone.png")

df1 %>%
  dplyr::filter(group == "Invasive species") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species", title = "Invasive species") + facet_wrap(~landUseZone, nrow = 1, labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black")) + ggsave("cache/figures/invasive_landUseZone.png")

df1 %>%
  dplyr::filter(group == "Weeds of national significance") %>% 
  ggplot( aes(x = year_range, y = percentages, group = status, colour = status)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Percentages of species", title = "Weeds of national significance") + facet_wrap(~landUseZone, nrow = 1, labeller = label_wrap_gen(width=10)) +
  scale_colour_manual(values = c("green4", "blue", "black")) + ggsave("cache/figures/wons_landUseZone.png")
