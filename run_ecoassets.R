# libraries, options, config -----
{
  library(conflicted)
  library(tidyverse)
  library(galah)
  library(arrow)
  library(assertthat)
  library(pointblank)
  library(ggnewscale)
  library(rmapshaper)
  library(magick)
  library(showtext)
  library(sf)
  library(xml2)
  library(here)
  library(httr)
  library(jsonlite)
}

list.files(path = "R", full.names = TRUE) |> 
  walk(source, verbose = FALSE)

conflicts_prefer(
  dplyr::filter,
  tidyr::unnest,
  dplyr::desc
)

galah_config(email = Sys.getenv("ALA_EMAIL"), verbose = TRUE)

options(arrow.pull_as_vector = TRUE)

sf_use_s2(FALSE)

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)

# get data ----- 
years <- as.numeric(c(1900:2023))
walk(years, get_occ)

# get data sources -------
galah_call() |> 
  galah_apply_profile(ALA) |>
  filter(year >= 1900,
         year <= 2023,
         decimalLatitude != "",
         decimalLongitude != "",
         speciesID != "",
         (!is.na(cl966) | !is.na(cl1048))) |>
  group_by(dataResourceName) |>
  atlas_counts() |> 
  write_csv("data/data_sources_spp_occ.csv")

galah_call() |> 
  galah_apply_profile(ALA) |>
  filter(year >= 2010,
         year <= 2023,
         decimalLatitude != "",
         decimalLongitude != "",
         speciesID != "",
         (!is.na(cl966) | !is.na(cl1048))) |>
  group_by(dataResourceName) |>
  atlas_counts() |> 
  write_csv("data/data_sources_env_monitoring.csv")

# check data -----
ds <- open_dataset("data/galah")
assert_that(are_equal(ncol(ds), 33))
assert_that(noNA(pull(ds, year)))
assert_that(noNA(pull(ds, decimalLatitude)))
assert_that(noNA(pull(ds, decimalLongitude)))
assert_that(noNA(pull(ds, speciesID)))
# TODO: loop this over all columns
# assert_that(not_all_NA(ds, "cl10000"))
# lapply(names(ds)[1:3], not_all_NA, ds = ds)

# Probably not possible to functionalise these scripts; each output needs to be
# manually examined and decisions have to be made about how to rub subsequent
# chunks of code based on those outputs. Some of the biodiversity code could be
# turned into functions though to make the scripts more readable

# monitoring datasets and plots 
source("scripts/assemble_monitoring.R")
source("scripts/plot_monitoring.R")

# biodiversity datasets and plots
source("scripts/create_relational_tables.R")
source("scripts/facet_biodiversity.R")
source("scripts/plot_biodiversity.R")

# state of forests datasets for ABARES
source("scripts/state_of_forests.R")