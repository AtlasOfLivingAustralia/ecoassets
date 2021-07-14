library(tidyverse)
library(patchwork)
library(data.table)
library(sf)
library(patchwork)

# Reading biome
biome <- st_read("ibra7_biomes_dissolve/ibra7_biomes_dissolve.shp")
biome <- st_transform(biome, 4326)
biome <- st_make_valid(biome)

# Reading IBRA shape file
ibra <- st_read("IBRA/ibra7_regions.shp")
ibra <- st_transform(ibra, 4326)
ibra <- st_make_valid(ibra)

int <- st_intersection(ibra, biome)

head(int)

int <- int %>% 
  dplyr::select(REG_NAME_7, B_NAME)
int$geometry <- NULL
head(int)

fwrite(int, "ibra_biome_int.csv")

biome_ibra <- fread("ibra_biome_int.csv")
colnames(biome_ibra) <- c("ibra", "biome")

#epbc
epbc <- fread("epbc.csv")
head(epbc)

epbc1 <- epbc %>%
  tidyr::pivot_longer(
    cols = c("overall", "inside_pa", "outside_pa"),
    names_to = "group",
    values_to = "percentages")
head(epbc1)

epbc_merged <- merge(epbc1, biome_ibra, by = "ibra", allow.cartesian = TRUE)
head(epbc_merged)

epbc_merged1 <- epbc_merged %>% 
  group_by(year_range, group, biome) %>% 
  summarise(percent = sum(percentages))
head(epbc_merged1)

g1 <- ggplot(epbc_merged1) +
  geom_line(aes(x = year_range, y = percent, group = group, colour = group)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("% of species") + facet_wrap(~biome) + ggtitle("EPBC")




#introduced
introduced <- fread("introduced.csv")
head(introduced)

introduced1 <- introduced %>%
  tidyr::pivot_longer(
    cols = c("overall", "inside_pa", "outside_pa"),
    names_to = "group",
    values_to = "percentages")
head(introduced1)

introduced_merged <- merge(introduced1, biome_ibra, by = "ibra", allow.cartesian = TRUE)
head(introduced_merged)

introduced_merged1 <- epbc_merged %>% 
  group_by(year_range, group, biome) %>% 
  summarise(percent = sum(percentages))
head(epbc_merged1)




#invasive
invasive <- fread("invasive.csv")
head(invasive)

invasive1 <- invasive %>%
  tidyr::pivot_longer(
    cols = c("overall", "inside_pa", "outside_pa"),
    names_to = "group",
    values_to = "percentages")
head(invasive1)

invasive_merged <- merge(invasive1, biome_ibra, by = "ibra", allow.cartesian = TRUE)
head(invasive_merged)

invasive_merged1 <- invasive_merged %>% 
  group_by(year_range, group, biome) %>% 
  summarise(percent = sum(percentages))
head(invasive_merged1)


#wons
wons <- fread("wons.csv")
head(wons)

wons1 <- wons %>%
  tidyr::pivot_longer(
    cols = c("overall", "inside_pa", "outside_pa"),
    names_to = "group",
    values_to = "percentages")
head(wons1)

wons_merged <- merge(wons1, biome_ibra, by = "ibra", allow.cartesian = TRUE)
head(wons_merged)

wons_merged1 <- wons_merged %>% 
  group_by(year_range, group, biome) %>% 
  summarise(percent = sum(percentages))
head(wons1)


# line chart
g1 <- ggplot(introduced_merged1) +
  geom_line(aes(x = year_range, y = percent, group = group, colour = group)) +
  theme_bw(base_size = 5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(), legend.position = "none") +
  xlab("") + ylab("% of species") + facet_wrap(~biome) + ggtitle("Introduced species")

g2 <- ggplot(introduced_merged1) +
  geom_line(aes(x = year_range, y = percent, group = group, colour = group)) +
  theme_bw(base_size = 5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(), legend.position = "none") +
  xlab("") + ylab("% of species") + facet_wrap(~biome) + ggtitle("Introduced species")

g3 <- ggplot(invasive_merged1) +
  geom_line(aes(x = year_range, y = percent, group = group, colour = group)) +
  theme_bw(base_size = 5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(), legend.position = "none") +
  xlab("") + ylab("% of species") + facet_wrap(~biome) + ggtitle("Invasive species")

g4 <- ggplot(wons_merged1) +
  geom_line(aes(x = year_range, y = percent, group = group, colour = group)) +
  theme_bw(base_size = 5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank()) +
  xlab("") + ylab("% of species") + facet_wrap(~biome) + ggtitle("WoNS")

(g1|g2)/(g3|g4)

ggsave("Trends.png")