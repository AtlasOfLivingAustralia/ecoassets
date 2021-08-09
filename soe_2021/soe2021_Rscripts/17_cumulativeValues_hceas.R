library(tidyverse)
library(data.table)

# Reading data file
landUseZone <- fread("cache/sumTable/landUseZone_merged.csv")

# Changing column names
colnames(landUseZone)[8] <- "sppInPa"
colnames(landUseZone)[13] <- "sppOutPa"
colnames(landUseZone)[12] <- "sppInPa_wons"
colnames(landUseZone)[17] <- "sppOutPa_wons"

landUseZone <- landUseZone %>% 
  dplyr::select(landUseZone, YearRange, sppBylandUseZone, sppBylandUseZone_epbc, 
                sppBylandUseZone_introduced, sppBylandUseZone_invasive, sppBylandUseZone_wons)

# Adding counts to match the land-use zones
landUseZone <- landUseZone %>% 
  group_by(landUseZone, YearRange) %>% 
  summarise(sppBylandUseZone = sum(sppBylandUseZone),
            sppBylandUseZone_epbc = sum(sppBylandUseZone_epbc),
            sppBylandUseZone_introduced = sum(sppBylandUseZone_introduced),
            sppBylandUseZone_invasive = sum(sppBylandUseZone_invasive),
            sppBylandUseZone_wons = sum(sppBylandUseZone_wons))


# Subset data frames for the calculation purpose
overall <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone)
overall <- overall %>%
  group_by(landUseZone) %>%
  mutate(csum=cumsum(sppBylandUseZone),
         group = "EPBC listed species")
overall <- overall %>%
  dplyr::select(landUseZone, YearRange, csum, group)

###################
epbc <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_epbc)
epbc <- epbc %>%
  group_by(landUseZone) %>%
  mutate(csum = cumsum(sppBylandUseZone_epbc),
         group = "EPBC listed species")
epbc <- epbc %>%
  dplyr::select(landUseZone, YearRange, csum, group)

###################
introduced <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_introduced)
introduced <- introduced %>%
  group_by(landUseZone) %>%
  mutate(csum = cumsum(sppBylandUseZone_introduced),
         group = "Introduced species")
introduced <- introduced %>%
  dplyr::select(landUseZone, YearRange, csum, group)

###################
invasive <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_invasive)
invasive <- invasive %>%
  group_by(landUseZone) %>%
  mutate(csum = cumsum(sppBylandUseZone_invasive),
         group = "Invasive species")
invasive <- invasive %>%
  dplyr::select(landUseZone, YearRange, csum, group)

###################
wons <- landUseZone %>%
  dplyr::select(landUseZone, YearRange, sppBylandUseZone_wons)
wons <- wons %>%
  group_by(landUseZone) %>%
  mutate(csum = cumsum(sppBylandUseZone_wons),
         group = "Weeds of national significance")
wons <- wons %>%
  dplyr::select(landUseZone, YearRange,  csum, group)

df <- rbind(epbc, introduced, invasive, wons)

write.csv(df, "cache/cumulativeValues_landUseZone.csv")

##############################################
# Sample figures

# Bar chart
df %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = YearRange, y = csum, group = landUseZone)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Number of species", title = "EPBC listed species") + facet_wrap(~landUseZone, nrow = 1, labeller = label_wrap_gen(width=10))

# Heat map
df %>%
  dplyr::filter(group == "EPBC listed species") %>% 
  ggplot( aes(x = YearRange, y = landUseZone, fill = csum)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Number of species", title = "EPBC listed species") + 
  scale_fill_viridis_c()

