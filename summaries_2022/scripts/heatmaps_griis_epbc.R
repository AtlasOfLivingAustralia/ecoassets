
# figure out why IMCRA has lots of white cells - NaN?

library(readr)
library(here)
library(dplyr)
library(purrr)
library(ggplot2)
library(showtext)
library(stringr)


# IBRA griis -----
# |- data ----
ibra_griis <- read_csv(here("summaries_2022", 
                            "data",
                            "processed",
                            "ibra_griis_5year.csv")) 


# |- recode and get proportions of invasive/introduced species -----
ibra_invasive <- ibra_griis |> 
  mutate(period = case_when(
    yearStart == 1900 ~ "1900-1970",
    yearStart == 1971 ~ "1970-1975",
    yearStart == 1976 ~ "1976-1980",
    yearStart == 1981 ~ "1981-1985",
    yearStart == 1986 ~ "1986-1990",
    yearStart == 1991 ~ "1991-1995",
    yearStart == 1996 ~ "1996-2000",
    yearStart == 2001 ~ "2001-2005",
    yearStart == 2006 ~ "2006-2010",
    yearStart == 2011 ~ "2011-2015",
    yearStart == 2016 ~ "2016-2020")) |> 
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


# |- pre-plotting ----
font_add_google("Lato", "lato")
showtext_auto()


# |- plot ----- 
hist(ibra_invasive$prop_invasive)       # skewed
hist(log(ibra_invasive$prop_invasive))  # normal(ish)
hist(ibra_invasive$speciesCount)
hist(log(ibra_invasive$speciesCount))


# heatmap of proportional values
p_prop <- ggplot(data = ibra_invasive,
       aes(x = period,
           y = reorder(ibraRegion, desc(ibraRegion)),
           fill = prop_invasive)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Proportion of introduced/invasive\nspecies in IBRA regions",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))


ggsave(here("summaries_2022",
            "plots",
            "heatmap_ibra_griis_prop.png"),
       p_prop, height = 16, width = 8, units = "in")


# heatmap of absolute values
p_abs <- ggplot(data = ibra_invasive,
                 aes(x = period,
                     y = reorder(ibraRegion, desc(ibraRegion)),
                     fill = speciesCount)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Number of introduced/invasive\nspecies in IBRA regions",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(n = 6),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave(here("summaries_2022",
            "plots",
            "heatmap_ibra_griis_abs.png"),
       p_abs, height = 16, width = 8, units = "in")




# IBRA epbc -----
# |- data ----
ibra_epbc <- read_csv(here("summaries_2022", 
                            "data",
                            "processed",
                            "ibra_epbc_5year.csv")) 


# |- recode and get proportions of threatened species -----
ibra_listed <- ibra_epbc |> 
  mutate(period = case_when(
    yearStart == 1900 ~ "1900-1970",
    yearStart == 1971 ~ "1970-1975",
    yearStart == 1976 ~ "1976-1980",
    yearStart == 1981 ~ "1981-1985",
    yearStart == 1986 ~ "1986-1990",
    yearStart == 1991 ~ "1991-1995",
    yearStart == 1996 ~ "1996-2000",
    yearStart == 2001 ~ "2001-2005",
    yearStart == 2006 ~ "2006-2010",
    yearStart == 2011 ~ "2011-2015",
    yearStart == 2016 ~ "2016-2020")) |> 
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


# |- pre-plotting ----
font_add_google("Lato", "lato")
showtext_auto()


# |- plot ----- 
hist(ibra_listed$prop_listed)       # skewed
hist(log(ibra_listed$prop_listed))  # normal(ish)
hist(ibra_listed$speciesCount)       
hist(log(ibra_listed$speciesCount))


# heatmap of proportional values
p_prop <- ggplot(data = ibra_listed,
            aes(x = period,
                y = reorder(ibraRegion, desc(ibraRegion)),
                fill = prop_listed)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Proportion of EPBC-listed\nspecies in IBRA regions",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))


ggsave(here("summaries_2022",
            "plots",
            "heatmap_ibra_epbc_prop.png"),
       p_prop, height = 16, width = 8, units = "in")


# heatmap of absolute values
p_abs <- ggplot(data = ibra_listed,
                 aes(x = period,
                     y = reorder(ibraRegion, desc(ibraRegion)),
                     fill = speciesCount)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Number of EPBC-listed\nspecies in IBRA regions",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(n = 6),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))


ggsave(here("summaries_2022",
            "plots",
            "heatmap_ibra_epbc_abs.png"),
       p_abs, height = 16, width = 8, units = "in")


# IMCRA griis-----
# |- data ----
imcra_griis <- read_csv(here("summaries_2022", 
                            "data",
                            "processed",
                            "imcra_griis_5year.csv")) 


# |- recode and get proportions of invasive/introduced species -----
imcra_invasive <- imcra_griis |> 
  mutate(period = case_when(
    yearStart == 1900 ~ "1900-1970",
    yearStart == 1971 ~ "1970-1975",
    yearStart == 1976 ~ "1976-1980",
    yearStart == 1981 ~ "1981-1985",
    yearStart == 1986 ~ "1986-1990",
    yearStart == 1991 ~ "1991-1995",
    yearStart == 1996 ~ "1996-2000",
    yearStart == 2001 ~ "2001-2005",
    yearStart == 2006 ~ "2006-2010",
    yearStart == 2011 ~ "2011-2015",
    yearStart == 2016 ~ "2016-2020")) |> 
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


# |- pre-plotting ----
font_add_google("Lato", "lato")
showtext_auto()


# |- plot ----- 
hist(imcra_invasive$prop_invasive)
hist(log(imcra_invasive$prop_invasive))
hist(imcra_invasive$speciesCount)
hist(log(imcra_invasive$speciesCount))


# heatmap of proportional values
p_prop <- ggplot(data = imcra_invasive,
            aes(x = period,
                y = reorder(imcraRegion, desc(imcraRegion)),
                fill = prop_invasive)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Proportion of introduced/invasive\nspecies in IMCRA regions",
                       type = "seq",
                       palette = "BuPu",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave(here("summaries_2022",
            "plots",
            "heatmap_imcra_griis_prop.png"),
       p_prop, height = 10, width = 8, units = "in")



# heatmap of absolute values
p_abs <- ggplot(data = imcra_invasive,
                 aes(x = period,
                     y = reorder(imcraRegion, desc(imcraRegion)),
                     fill = speciesCount)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Number of introduced/invasive\nspecies in IMCRA regions",
                       type = "seq",
                       palette = "BuPu",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(n = 6),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave(here("summaries_2022",
            "plots",
            "heatmap_imcra_griis_abs.png"),
       p_abs, height = 10, width = 8, units = "in")


# IMCRA epbc -----
# |- data ----
imcra_epbc <- read_csv(here("summaries_2022", 
                           "data",
                           "processed",
                           "imcra_epbc_5year.csv")) 


# |- recode and get proportions of threatened species -----
imcra_listed <- imcra_epbc |> 
  mutate(period = case_when(
    yearStart == 1900 ~ "1900-1970",
    yearStart == 1971 ~ "1970-1975",
    yearStart == 1976 ~ "1976-1980",
    yearStart == 1981 ~ "1981-1985",
    yearStart == 1986 ~ "1986-1990",
    yearStart == 1991 ~ "1991-1995",
    yearStart == 1996 ~ "1996-2000",
    yearStart == 2001 ~ "2001-2005",
    yearStart == 2006 ~ "2006-2010",
    yearStart == 2011 ~ "2011-2015",
    yearStart == 2016 ~ "2016-2020")) |> 
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


# |- pre-plotting ----
font_add_google("Lato", "lato")
showtext_auto()


# |- plot ----- 
hist(imcra_listed$prop_listed)       # skewed
hist(log(imcra_listed$prop_listed))  # normal(ish)
hist(imcra_listed$speciesCount)       
hist(log(imcra_listed$speciesCount))


# heatmap of proportional values
p_prop <- ggplot(data = imcra_listed,
                 aes(x = period,
                     y = reorder(imcraRegion, desc(imcraRegion)),
                     fill = prop_listed)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Proportion of EPBC-listed\nspecies in IMCRA regions",
                       type = "seq",
                       palette = "BuPu",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave(here("summaries_2022",
            "plots",
            "heatmap_imcra_epbc_prop.png"),
       p_prop, height = 10, width = 8, units = "in")


# heatmap of absolute values
p_abs <- ggplot(data = imcra_listed,
                 aes(x = period,
                     y = reorder(imcraRegion, desc(imcraRegion)),
                     fill = speciesCount)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_distiller(name = "Number of EPBC-listed\nspecies in IMCRA regions",
                       type = "seq",
                       palette = "BuPu",
                       direction = 1,
                       trans = "log",
                       breaks = scales::breaks_log(n = 6),
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              draw.ulim = FALSE, 
                                              draw.llim = FALSE,
                                              title.position = "top",
                                              ticks = FALSE,
                                              barwidth = 16)) +
  
  theme_classic() +
  theme(text = element_text(family = "lato"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave(here("summaries_2022",
            "plots",
            "heatmap_imcra_epbc_abs.png"),
       p_abs, height = 10, width = 8, units = "in")

