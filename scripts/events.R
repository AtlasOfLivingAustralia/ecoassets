# Script wrangling ALA, TERN, and IMOS data into event-like monitoring datasets.
# Spatial facets are represented by IBRA and IMCRA regions, and temporal facets 
# by calendar years from 2010 onwards. Keywords are based on 
# data/external/events_mapping.csv from DH, which is based loosely on GCMD

library(tidyverse)
library(lubridate)
library(sf)
library(xml2)
library(arrow)
library(galah)


# spatial layers for joins -----
imcra_sf <- read_sf("data/external/imcra4/imcra4_meso.shp") |> 
  st_transform(4326) |> 
  select(region = MESO_NAME)

ibra_sf <- read_sf("data/external/ibra7/ibra7_regions.shp") |> 
  st_transform(4326) |> 
  select(region = REG_NAME_7)

states_sf <- read_sf("data/external/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp") |> 
  st_transform(4326) |> 
  filter(STE_NAME21 != "Outside Australia") |> 
  select(STE_NAME21)

sf_use_s2(FALSE)


### TERN ### -----
# For TERN, generate one row for each combination of a year identified in the 
# pipe-delimited visit_date column in the CSV file, the IBRA region indicated by
# the coordinates and the TERN keywords included as features_of_interest. If a 
# site lacks any visit_dates, use the commissioned_date and all subsequent years

# data was sent as a csv from TERN
tern_csv <- read_csv("data/external/tern-20230323.csv")

# TERN dates ------
without_visit_date <- tern_csv |> 
  filter(is.na(visit_date)) |> 
  filter(!is.na(date_commissioned)) |> 
  mutate(year_start = year(dmy(date_commissioned)),
         year_end = 2022) |> 
  rownames_to_column() |>  
  nest(data = c(year_start, year_end)) |> 
  mutate(year = map(data, ~ seq(.x$year_start, .x$year_end, by = 1))) |> 
  select(-data, -rowname) |> 
  unnest(cols = year)

ongoing <- tern_csv |> 
  filter(visit_date == "ongoing") |> 
  mutate(year_start = year(dmy(date_commissioned)),
         year_end = 2022) |> 
  rownames_to_column() |>  
  nest(data = c(year_start, year_end)) |> 
  mutate(year = map(data, ~ seq(.x$year_start, .x$year_end, by = 1))) |> 
  select(-data, -rowname) |> 
  unnest(cols = year)
  
tern_dates <- tern_csv |> 
  filter(!is.na(visit_date)) |> 
  filter(visit_date != "ongoing") |> 
  separate_longer_delim(visit_date, delim = "|") |> 
  mutate(year = year(dmy(visit_date))) |> 
  bind_rows(without_visit_date, ongoing) |> 
  select(-date_commissioned, -visit_date) |> 
  filter(year >= 2010)


# TERN spatial------
tern_spatial <- tern_dates |> 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326,
           remove = FALSE) |> 
  st_join(ibra_sf, join = st_intersects) |>
  # removes stations in NZ
  filter(!is.na(region)) |>
  st_join(states_sf, join = st_intersects) |> 
  mutate(datasetName = paste0(type_name, " ", name)) |> 
  select(year,
         ibraRegion = region, 
         stateTerritory = STE_NAME21,
         decimalLatitude = lat,
         decimalLongitude = long,
         features_of_interest,
         datasetURI = uri,
         datasetName) |>
  st_set_geometry(NULL)
  

# TERN keywords -----
mapping_tern <- read_csv("data/external/events_mapping.csv") |> 
  filter(source == "TERN") |> 
  mutate(keyword = tolower(keyword)) |> 
  unique()

# manually change some keywords so they map more cleanly to the provided option
tern_events <- tern_spatial |> 
  separate_longer_delim(features_of_interest, delim = " | ") |> 
  mutate(features_of_interest = tolower(features_of_interest),
         features_of_interest = case_when(
           features_of_interest == "fauna population" ~ "animal population",
           features_of_interest == "flux" ~ "climate",
           features_of_interest == "soil surface" ~ "soil horizon",
           TRUE ~ features_of_interest)) |> 
  left_join(mapping_tern, join_by(features_of_interest == keyword)) |> 
  rowwise() |> 
  mutate(facet2 = case_when(!is.na(facet2) ~ facet2,
                            is.na(facet2) ~ facet1),
         facet3 = case_when(!is.na(facet3) ~ facet3,
                            is.na(facet3) ~ facet2)) |> 
  ungroup() |> 
  select(nri = source,
         datasetName,
         datasetURI, 
         nriKeyword = features_of_interest, 
         decimalLatitude,
         decimalLongitude,
         year, 
         stateTerritory,
         ibraRegion,
         featureID = id, 
         featureName = label, 
         featureFacet1 = facet1,
         featureFacet2 = facet2, 
         featureFacet3 = facet3) 

saveRDS(tern_events, "data/interim/tern_events.RDS")


### IMOS ### ------
# For IMOS, generate one row for each combination of a calendar year (either 
# the single year of data collection or every year from start to end year), the 
# IMCRA regions intersecting the bounding box for the dataset and the GCMD 
# keywords for the dataset for each dataset in AODN with metadata fitting these
# criteria. If a dataset has three keywords, intersects five IMCRA regions, and 
# spans a two-year period, we will end up with 30 rows for that dataset (3 * 5 * 2).

# get raw data -------
# get records from first page
# extract elements of interest for 1:n records in the first page
# get the resumption token for the next page
# get records from the second page, continue until no resumption token is available

# records from page 1
initial_url <- "https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/oaipmh?verb=ListRecords&metadataPrefix=iso19115-3.2018"
xml <- read_xml(initial_url)
ns <- xml_ns(xml)
recs_pg1 <- xml_find_all(xml, "//d1:record", ns)

# pages 2:n
all_pages <- list()
token <- xml_text(xml_find_all(xml, "//d1:resumptionToken", ns)) 

while (length(token) != 0) {
  
  print(token)
  xml <- read_xml(paste0("https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/oaipmh?verb=ListRecords&resumptionToken=", token))
  records <- xml_find_all(xml, "//d1:record", xml_ns(xml))
  all_pages[[length(all_pages)+1]] <- records
  token <- xml_text(xml_find_all(xml, "//d1:resumptionToken", ns)) 
  
}

# add results from page 1
all_pages[[length(all_pages)+1]] <- recs_pg1

# flatten the list by a layer
all_records <- flatten(all_pages)

# for iterating over all records
paths <- c(".//gex:northBoundLatitude//gco:Decimal",
           ".//gex:southBoundLatitude//gco:Decimal",
           ".//gex:eastBoundLongitude//gco:Decimal",
           ".//gex:westBoundLongitude//gco:Decimal",
           ".//gex:EX_TemporalExtent//gml:beginPosition",
           ".//gex:EX_TemporalExtent//gml:endPosition",
           ".//mcc:MD_Identifier//gco:CharacterString",
           ".//mdb:metadataLinkage//cit:linkage//gco:CharacterString",
           ".//mri:citation//cit:title//gco:CharacterString",
           #".//mri:abstract//gco:CharacterString",
           ".//mri:MD_Keywords/mri:keyword/gco:CharacterString")

fields <- c("north", 
            "south",
            "east",
            "west",
            "start_period",
            "end_period",
            "identifier",
            "metadata",
            "citation",
            #"abstract",
            "keywords")

# easier to parse than an anonymous function within a nested purrr::map call
get_values <- function(records, path) {
  map(records, ~ xml_find_all(.x, path, ns) |> xml_text())
}

# get the data
imos_data <- paths |> 
  set_names(fields) |> 
  map(~get_values(records = all_records, path = .x)) |> 
  enframe() |> 
  pivot_wider(names_from = name, values_from = value) |>
  unnest(cols = everything(), keep_empty = TRUE) |>
  unnest(cols = c(north, south, east, west, citation,
                  start_period, end_period, metadata),
         keep_empty = TRUE) 

# saveRDS(imos_data, "data/raw/imos_raw_eventdata.RDS")
# imos_data <- readRDS("data/raw/imos_raw_eventdata.RDS")

# IMOS dates ------
imos_dates <- imos_data |> 
  separate_wider_delim(start_period, delim = "T", names_sep = "", too_few = "align_start") |> 
  separate_wider_delim(end_period, delim = "T", names_sep = "", too_few = "align_start") |> 
  select(-start_period2, -end_period2) |> 
  mutate(start_year = year(parse_date_time(start_period1, c("ymd", "ym"))),
         end_year = year(parse_date_time(end_period1, c("ymd", "ym"))),
         inferred_end_year = case_when(
           is.na(end_year) ~ 2023,
           !is.na(end_year) ~ end_year),
         inferred_dq_issue = case_when(
           inferred_end_year >= start_year ~ "correct",
           inferred_end_year < start_year ~ "error")) |> 
  filter(!is.na(start_year),
         inferred_dq_issue == "correct") |> 
  rownames_to_column() |>
  nest(data = c(start_year, inferred_end_year)) |> 
  mutate(year = map(data, ~ seq(.x$start_year, .x$inferred_end_year, by = 1))) |> 
  select(-c(data, rowname, end_year, start_period1, 
            end_period1, inferred_dq_issue)) |> 
  unnest(cols = year) |> 
  filter(year >= 2010)

# IMOS spatial --------
imos_sf <- imos_dates |> 
  filter(!is.na(north)) |> 
  mutate(across(north:west, as.numeric)) |> 
  rowwise() |> 
  mutate(decimalLatitude = case_when((north == south) & (east == west) ~ north,
                                     TRUE ~ NA),
         decimalLongitude = case_when((north == south) & (east == west) ~ east,
                                      TRUE ~ NA),
         polygon = st_as_sfc(st_bbox(c(xmin = west,
                                       xmax = east,
                                       ymin = south,
                                       ymax = north),
                                     crs = st_crs(4326)))) |> 
  ungroup() |> 
  st_as_sf(crs = 4326, sf_column_name = "polygon", remove = FALSE) |> 
  st_join(imcra_sf, join = st_intersects) |> 
  st_join(states_sf, join = st_intersects) |> 
  st_drop_geometry()

# IMOS keywords -------
mapping_imos <- read_csv("data/external/events_mapping.csv") |> 
  filter(source == "IMOS") |> 
  mutate(keyword = tolower(keyword)) |> 
  unique()

imos_events <- imos_sf |> 
  filter(!is.na(region)) |> 
  unnest(cols = keywords) |> 
  mutate(keywords = tolower(keywords)) |> 
  inner_join(mapping_imos, join_by(keywords == keyword)) |> 
  mutate(facet2 = case_when(!is.na(facet2) ~ facet2,
                            is.na(facet2) ~ facet1),
         facet3 = case_when(!is.na(facet3) ~ facet3,
                            is.na(facet3) ~ facet2)) |> 
  select(nri = source,
         datasetName = citation,
         datasetURI = metadata, 
         nriKeyword = keywords, 
         decimalLatitude,
         decimalLongitude,
         year, 
         stateTerritory = STE_NAME21,
         imcraRegion = region,
         featureID = id, 
         featureName = label, 
         featureFacet1 = facet1,
         featureFacet2 = facet2, 
         featureFacet3 = facet3)

saveRDS(imos_events, "data/interim/imos_events.RDS")


### ALA ### -------

ds <- open_dataset("data/galah") 

# Find all occurrences associated with events from 2010 onwards and falling in 
# an IBRA or IMCRA region and having a species identification or better
# For each event, get the dataset id, samplingProtocol, coordinates, date, 
# number of taxa included, the number of occurrences included and min-left and 
# max-right - these are your event records

event_records <- ds |> 
  filter(year >= 2010,
         !is.na(cl1048) | !is.na(cl966),
         #!is.na(samplingProtocol),
         !is.na(eventID),
         !is.na(dataResourceUid)) |>
  mutate(samplingProtocol = tolower(samplingProtocol), 
         eventID = tolower(eventID)) |> 
  group_by(decimalLatitude,
           decimalLongitude,
           cl22, 
           cl1048,
           cl966,
           year, 
           eventID,
           dataResourceUid,
           dataResourceName, 
           samplingProtocol) |> 
  summarise(occCount = n(),
            speciesCount = n_distinct(species),
            minLeft = min(lft),
            maxRight = max(rgt)) |> 
  ungroup() |> 
  rename(ibraRegion = cl1048,
         imcraRegion = cl966,
         stateTerritory = cl22) |> 
  collect()


# Group these by unique dataset id and samplingProtocol and get the dataset id, 
# sampling protocol, number of events, maximum number of species per event, maximum number 
# of species per event, min-min-left and max-max-right - these are your protocol records

protocol_records <- event_records |> 
  group_by(dataResourceUid,
           dataResourceName, 
           samplingProtocol, eventID) |> 
  summarise(eventsCount = n(),
            maxSpeciesCount = max(speciesCount),
            maxOccCount = max(occCount),
            minMinLeft = min(minLeft),
            maxMaxRight = max(maxRight)) |> 
  ungroup() 

# check left values always less than or equal to right values (should be 0)
protocol_records |> 
  mutate(left_right_check = if_else(minMinLeft <= maxMaxRight, "correct", "wrong")) |> 
  filter(left_right_check == "wrong")
  

# Throw away all protocol records in any of the following groups:
# Number of events less than 10
# Maximum number of species per event less than 5
# Min-min-left and max-max-right span more than one kingdom AND 
# the events include animal records

kingdoms_lft_rgt <- readRDS("data/interim/kingdoms_lft_rgt.RDS")

protocol_records_filtered <- protocol_records |> 
  filter(eventsCount >= 10,
         maxSpeciesCount >= 5) |> 
  left_join(kingdoms_lft_rgt, 
            join_by(between(minMinLeft, lft, rgt, bounds = "[]"))) |> 
  select(-c(lft, rgt, rank)) |> 
  rename(kingdom_left = search_term) |> 
  left_join(kingdoms_lft_rgt, 
            join_by(between(maxMaxRight, lft, rgt, bounds = "[]"))) |>
  select(-c(lft, rgt, rank)) |> 
  rename(kingdom_right = search_term) |> 
  mutate(exclude = case_when(
    (kingdom_left != kingdom_right) & (kingdom_left == "Animalia" | kingdom_right == "Animalia") ~ "exclude",
    TRUE ~ "include")) |> 
  filter(exclude == "include") |> 
  select(-exclude)


# Map each protocol record to the best fit from the biodiversity terms in the 
# vocabulary. E.g. anything with just birds will be mapped to the 
# "Biodiversity | Animals | Vertebrates | Birds" concept. You should be able to
# do this with the left-right ranges for the ALA taxon concepts associated with 
# each vocabulary concept. If the protocol includes multiple non-animal kingdoms, 
# treat it as "Biodiversity | Plants" - this seems the best fit.

mapping_ala <- read_csv("data/external/events_mapping.csv") |>
  filter(str_detect(label, "Biodiversity")) |> 
  select(-source, -keyword) |> 
  unique()

# NZOR ids (haptophytes, coccolithophores, diatoms, bacteria) don't produce
# a match using search_taxa()
ala_keywords <- search_taxa(mapping_ala$id)

ala_lft_rgt <- mapping_ala |>
  full_join(ala_keywords,
            join_by("id" == "search_term")) |> 
  select(id:facet3, lft, rgt) |> 
  rowwise() |> 
  mutate(levels = str_count(label, "\\|")) |> 
  ungroup()

# classify animal and non-animal separately
protocol_records_non_animal <- protocol_records_filtered |> 
  filter(kingdom_left == "Plantae" | kingdom_right == "Plantae") |> 
  mutate(label = "Biodiversity | Plants") |> 
  left_join(ala_lft_rgt, by = join_by(label))

protocol_records_animal <- protocol_records_filtered |> 
  filter(kingdom_left == "Animalia") |> 
  left_join(ala_lft_rgt,
            join_by(within(minMinLeft, maxMaxRight, lft, rgt))) |> 
  group_by(dataResourceUid,
           dataResourceName,
           samplingProtocol,
           eventID,
           eventsCount,
           maxSpeciesCount,
           maxOccCount, 
           minMinLeft,
           maxMaxRight) |> 
  slice_max(levels, n = 1) |> 
  ungroup()
  
protocol_records_mapped <- bind_rows(protocol_records_animal, protocol_records_non_animal)

# Throw away all event records that do not match any of the 
# remaining protocol records 
# Generate a row for each event with IBRA/IMCRA region, 
# year and the vocabulary concept from the protocol record

event_records_matched <- event_records |> 
 inner_join(protocol_records_mapped, 
            by = join_by(dataResourceUid, 
                         dataResourceName, 
                         samplingProtocol,
                         eventID),
            na_matches = "na",
            multiple = "warning")

ala_events <- event_records_matched |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326,
           remove = FALSE) |> 
  st_join(states_sf, join = st_intersects) |> 
  st_set_geometry(NULL) |> 
  mutate(nri = "ALA",
         datasetURI = paste0("https://collections.ala.org.au/public/show/", dataResourceUid),
         nriKeyword = paste0(samplingProtocol, ": ", eventsCount, " samples"),
         facet2 = case_when(!is.na(facet2) ~ facet2,
                            is.na(facet2) ~ facet1),
         facet3 = case_when(!is.na(facet3) ~ facet3,
                            is.na(facet3) ~ facet2),
         stateTerritory = case_when(!is.na(stateTerritory) ~ stateTerritory,
                                    is.na(stateTerritory) & !is.na(STE_NAME21) ~ STE_NAME21,
                                    is.na(stateTerritory) & is.na(STE_NAME21) ~ NA)) |> 
  select(nri,
         datasetName = dataResourceName,
         datasetURI, 
         nriKeyword, 
         decimalLatitude,
         decimalLongitude,
         year, 
         stateTerritory,
         ibraRegion,
         featureID = id, 
         featureName = label, 
         featureFacet1 = facet1,
         featureFacet2 = facet2, 
         featureFacet3 = facet3) 

saveRDS(ala_events, "data/interim/ala_events.RDS")  

# ALA left and right values: OPTION 01 -----
# needs to be re-run whenever names index changes
#
# library(galah)
# library(httr)
# library(jsonlite)
#
# url <- "https://namematching-ws.ala.org.au/api/search?q="  
# 
# kingdoms <- search_fields("kingdom") |> 
#   show_values() 
# 
# queries <- paste0(url, kingdoms$category)
# 
# get_lft_rgt <- function(query) {
#   x <- GET(query)
#   y <- fromJSON(rawToChar(x$content))
#   tibble(kingdom = y$kingdom,
#          left = y$lft, 
#          right = y$rgt, 
#          match_type = y$matchType,
#          issues = y$issues)
# }
# 
# kingdoms_lft_rgt <- map_dfr(queries, get_lft_rgt)
# 
# ALA left and right values: OPTION 02 -----
# use {galah} version on a branch to get lft and rgt values
# remotes::install_github("AtlasOfLivingAustralia/galah@lft-rgt")
# library(galah)
# kingdoms <- search_fields("kingdom") |>
#   show_values()
# get_lft_rgt <- search_taxa(kingdoms$category) |>
#   select(search_term, lft, rgt, rank)
# kingdoms_lft_rgt <- full_join(get_lft_rgt,
#                               kingdoms,
#                               join_by("search_term" == "category",
#                                       "rank" == "field"))
# saveRDS(kingdoms_lft_rgt, "data/interim/kingdoms_lft_rgt.RDS")


### main asset ### --------
tern_events <- readRDS("data/interim/tern_events.RDS")
imos_events <- readRDS("data/interim/imos_events.RDS")
ala_events <- readRDS("data/interim/ala_events.RDS")

events <- tern_events |> 
  bind_rows(imos_events, ala_events) |> 
  relocate(imcraRegion, .after = ibraRegion)

saveRDS(events, "data/processed/events.RDS")
write_csv(events, "data/processed/events.csv")


### derived assets: IBRA & IMCRA ### ---------
ibra_events <- events |> 
  filter(!is.na(ibraRegion)) |> 
  group_by(year,
           stateTerritory,
           ibraRegion,
           featureID,
           featureName,
           featureFacet1,
           featureFacet2,
           featureFacet3) |> 
  summarise(recordCount = n())

saveRDS(ibra_events, "data/processed/ibra_events.RDS")
write_csv(ibra_events, "data/processed/ibra_events.csv")

imcra_events <- events |> 
  filter(!is.na(imcraRegion)) |> 
  group_by(year,
           stateTerritory,
           imcraRegion,
           featureID,
           featureName,
           featureFacet1,
           featureFacet2,
           featureFacet3) |> 
  summarise(recordCount = n())

saveRDS(imcra_events, "data/processed/imcra_events.RDS")
write_csv(imcra_events, "data/processed/imcra_events.csv")
