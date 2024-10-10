# libraries -----
{
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
# TODO: source functions here 


# options / environment variables ----- 
galah_config(email = Sys.getenv("ALA_EMAIL"), verbose = TRUE)
options(arrow.pull_as_vector = TRUE)

years <- as.numeric(c(1900:2023))


# get data ----- 
walk(years, get_data)


# check data -----
ds <- open_dataset("data/galah")
assert_that(are_equal(ncol(ds), 33))
assert_that(noNA(pull(ds, year)))
assert_that(noNA(pull(ds, decimalLatitude)))
assert_that(noNA(pull(ds, decimalLongitude)))
assert_that(noNA(pull(ds, speciesID)))
# TODO: loop this over all columns
assert_that(not_all_NA(ds, "cl10000"))
lapply(names(ds)[1:3], not_all_NA, ds = ds)



# 3. monitoring datasets and plots 
source("scripts/assemble_monitoring.R")
source("scripts/plot_monitoring.R")

# 4. biodiversity datasets and plots
source("scripts/create_relational_tables.R")
source("scripts/facet_biodiversity.R")
source("scripts/plot_biodiversity.R")

# 5. state of forests datasets for ABARES
source("scripts/state_of_forests.R")