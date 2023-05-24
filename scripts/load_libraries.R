# Loads libraries necessary for running EcoAssets 

library(tidyverse)
library(galah)
library(arrow)
library(pointblank)
library(ggnewscale)
library(rmapshaper)
library(magick)
library(showtext)
library(sf)
library(xml2)
library(here)

galah_config(email = Sys.getenv("email"), verbose = TRUE)