
# replace mutate -----
# simplify repeated mutate calls to convert years
as_period <- function(df) {
  
  df |>
    mutate(
      period_key = case_when(
        year <= 1970 ~ 1,
        year >= 1971 & year <= 1975 ~ 2,
        year >= 1976 & year <= 1980 ~ 3,
        year >= 1981 & year <= 1985 ~ 4,
        year >= 1986 & year <= 1990 ~ 5,
        year >= 1991 & year <= 1995 ~ 6,
        year >= 1996 & year <= 2000 ~ 7,
        year >= 2001 & year <= 2005 ~ 8,
        year >= 2006 & year <= 2010 ~ 9,
        year >= 2011 & year <= 2015 ~ 10,
        year >= 2016 & year <= 2020 ~ 11))
  
}

as_start_end <- function(df) {
  
  df |> 
    mutate(
      yearStart = case_when(
        period_key == 1 ~ 1900, 
        period_key == 2 ~ 1971,
        period_key == 3 ~ 1976,
        period_key == 4 ~ 1981,
        period_key == 5 ~ 1986,
        period_key == 6 ~ 1991,
        period_key == 7 ~ 1996,
        period_key == 8 ~ 2001,
        period_key == 9 ~ 2006,
        period_key == 10 ~ 2011,
        period_key == 11 ~ 2016), 
      yearEnd = case_when(
        period_key == 1 ~ 1970,
        period_key == 2 ~ 1975,
        period_key == 3 ~ 1980,
        period_key == 4 ~ 1985,
        period_key == 5 ~ 1990,
        period_key == 6 ~ 1995,
        period_key == 7 ~ 2000,
        period_key == 8 ~ 2005,
        period_key == 9 ~ 2010,
        period_key == 10 ~ 2015,
        period_key == 11 ~ 2020))
  
}

as_period_string <- function(df) {
  
  df |>
    mutate(
      period = case_when(
        yearStart == 1900 ~ "1900-1970",
        yearStart == 1971 ~ "1971 - 1975",
        yearStart == 1976 ~ "1976 - 1980",
        yearStart == 1981 ~ "1981 - 1985",
        yearStart == 1986 ~ "1986 - 1990",
        yearStart == 1991 ~ "1991 - 1995",
        yearStart == 1996 ~ "1996 - 2000",
        yearStart == 2001 ~ "2001 - 2005",
        yearStart == 2006 ~ "2006 - 2010",
        yearStart == 2011 ~ "2011 - 2015",
        yearStart == 2016 ~ "2016 - 2020"))
      
}

# themes for plotting -----
plot_heatmap <- function(plot, legend_title, pal, n_breaks) {
 
  plot +
  geom_tile() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_distiller(name = legend_title,
                         type = "seq",
                         palette = pal,
                         direction = 1,
                         trans = "log",
                         breaks = scales::breaks_log(n = n_breaks),
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
  
}

# manual scale ensures legends are consistent across plots
# drop = FALSE in scale_fill_manual stops it from crashing when the data doesn't have 5 levels 
plot_choropleth <- function(imcra_dat, ibra_dat) {
  
  start_year <- unique(imcra_dat$yearStart)
  end_year <- unique(imcra_dat$yearEnd)
  
  ggplot() +
    geom_sf(data = imcra_dat,
            aes(fill = prop_discrete),
            colour = NA) +
    scale_fill_manual(name = "IMCRA",
                      drop = FALSE,
                      labels = c("0.2", "0.4", "0.6", "0.8"),
                      # 5-class BuPu
                      values = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c"),
                      guide = guide_colorsteps(
                        direction = "horizontal",
                        label.position = "bottom",
                        title.position = "left",
                        title.vjust = 0.8, 
                        title.hjust = 0.8)) +
    new_scale_fill() +
    geom_sf(data = ibra_dat,
            aes(fill = prop_discrete),
            colour = NA) +
    scale_fill_manual(name = "IBRA",
                      drop = FALSE,
                      labels = c("0.2", "0.4", "0.6", "0.8"),
                      # 5-class YlOrBr
                      values = c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404"),
                      guide = guide_colorsteps(
                        direction = "horizontal",
                        label.position = "bottom",
                        title.position = "left",
                        title.vjust = 0.8, 
                        title.hjust = 0.8)) +
    annotate("text", 
             x = 133,
             y = -45.5,
             label = str_glue("Proportion of human observations: {start_year} - {end_year}"),
             size = 8) +
    coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
    theme_void() +
    theme(text = element_text(family = "lato"),
          title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.key.width = unit(12, 'mm'),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white')) 
}

plot_monitoring_effort <- function(ibra_dat, imcra_dat) {
  
  ggplot() +
    geom_sf(data = ibra_dat,
            aes(fill = intensity),
            colour = NA) +
    scale_fill_gradient(name = "IBRA",
                        low = "#ccd2ab",
                        high = "#15521d",
                        limits = c(0, 4),
                        breaks = c(0, 1, 2, 3, 4), 
                        guide = guide_colorbar(draw.ulim = FALSE,
                                               draw.llim = FALSE,
                                               ticks = FALSE,
                                               direction = "horizontal",
                                               label.position = "bottom",
                                               title.position = "left",
                                               title.vjust = 0.85,
                                               title.hjust = 0.85)) +
    new_scale_fill() +
    geom_sf(data = imcra_dat,
            aes(fill = intensity),
            colour = NA) +
    scale_fill_gradient(name = "IMCRA",
                        low = "#bed7ce",
                        high = "#286699",
                        limits = c(0, 4),
                        breaks = c(0, 1, 2, 3, 4), 
                        guide = guide_colorbar(draw.ulim = FALSE,
                                               draw.llim = FALSE,
                                               ticks = FALSE,
                                               direction = "horizontal",
                                               label.position = "bottom",
                                               title.position = "left",
                                               title.vjust = 0.85,
                                               title.hjust = 0.85)) +
    coord_sf(xlim = c(110, 155),
             ylim = c(-45, -2)) +
    annotate("text",
             x = 130,
             y = -3,
             label = "Environmental Monitoring and Observations Effort",
             size = 8) +
    annotate("text",
             x = 128.5,
             y = -5,
             label = "Coverage by major earth science facets (2010 - 2022)",
             size = 7) +
    annotate("text",
             x = 132.5,
             y = -45.5, 
             label = "Mean number of major earth science facets measured per year",
             size = 6) +
    theme_void() +
    theme(text = element_text(family = "lato"),
          title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.key.width = unit(12, 'mm'),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white'))
}

plot_sample_event_points <- function(ibra_sf, imcra_sf, point_data, point_col) {
  
  ggplot() +
    geom_sf(data = ibra_sf,
            fill = "#fee5b155",
            colour = "#fee5b1") +
    geom_sf(data = imcra_sf,
            fill = "#bed7ce55",
            colour = "#bed7ce") +
    geom_sf(data = point_data,
            shape = 16,
            colour = point_col,
            size = 0.8) +
    coord_sf(xlim = c(110, 155),
             ylim = c(-45, -5)) +
    theme_void() +
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.background = element_rect(fill = 'white', colour = 'white'))
}