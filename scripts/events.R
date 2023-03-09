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


### TERN ### -----
# For TERN, generate one row for each combination of a year identified in the 
# pipe-delimited visit_date column in the CSV file, the IBRA region indicated by
# the coordinates and the TERN keywords included as features_of_interest. If a 
# site lacks any visit_dates, use the commissioned_date and all subsequent years

# data was sent as a csv from TERN
tern_csv <- read_csv("data/external/TERN_Sites.csv")

# TERN dates ------
without_visit_date <- tern_csv |> 
  filter(is.na(visit_date)) |> 
  mutate(year_start = year(dmy(date_commissioned)),
         year_end = 2022) |> 
  rownames_to_column() |>  
  nest(data = c(year_start, year_end)) |> 
  mutate(year = map(data, ~ seq(.x$year_start, .x$year_end, by = 1))) |> 
  select(-data, -rowname) |> 
  unnest(cols = year)

tern_dates <- tern_csv |> 
  filter(!is.na(visit_date)) |> 
  separate_longer_delim(visit_date, delim = "|") |> 
  mutate(year = year(dmy(visit_date))) |> 
  bind_rows(without_visit_date) |> 
  select(-date_commissioned, -visit_date) |> 
  filter(year >= 2010)


# TERN spatial------
ibra_sf <- read_sf("data/external/ibra7/ibra7_regions.shp") |> 
  st_transform(4326) |> 
  select(REG_NAME_7)

sf_use_s2(FALSE)

tern_spatial <- tern_dates |> 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) |> 
  st_join(ibra_sf, join = st_intersects) |> 
  select(year,
         region = REG_NAME_7, 
         features_of_interest,
         datasetURI = uri,
         datasetName = name) |> 
  filter(!is.na(region)) |> 
  st_set_geometry(NULL)
  

# TERN keywords -----
mapping_tern <- read_csv("data/external/events_mapping.csv") |> 
  filter(source == "TERN") |> 
  mutate(keyword = tolower(keyword)) |> 
  unique()

# manually change some keywords so they map more cleanly to the provided option
tern_events <- tern_spatial |> 
  separate_longer_delim(features_of_interest, delim = "|") |> 
  mutate(features_of_interest = tolower(features_of_interest),
         features_of_interest = case_when(
           features_of_interest == "vegetationn fuel" ~ "vegetation fuel",
           features_of_interest == "fauna population" ~ "animal population",
           features_of_interest == "flux" ~ "climate",
           features_of_interest == "soil surface" ~ "soil horizon",
           features_of_interest == "vegetation decomposition" ~ "plant matter",
           TRUE ~ features_of_interest)) |> 
  left_join(mapping_tern, join_by(features_of_interest == keyword)) |> 
  select(NRI = source,
         datasetName,
         datasetURI, 
         keywords = features_of_interest, 
         year, 
         region,
         id, 
         label, 
         facet1,
         facet2, 
         facet3)

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
imcra_sf <- read_sf("data/external/imcra4/imcra4_meso.shp") |> 
  st_transform(4326) |> 
  select(MESO_NAME)

sf_use_s2(FALSE)

imos_sf <- imos_dates |> 
  filter(!is.na(north)) |> 
  mutate(across(north:west, as.numeric)) |> 
  rowwise() |> 
  mutate(polygon = st_as_sfc(
    st_bbox(c(xmin = west,
              xmax = east,
              ymin = south,
              ymax = north),
            crs = st_crs(4326)))) |> 
  ungroup() |> 
  select(-c(north, south, east, west, identifier)) |> 
  st_as_sf(crs = 4326, sf_column_name = "polygon") |> 
  st_join(imcra_sf, join = st_intersects) |> 
  st_drop_geometry()

# IMOS keywords -------

mapping_imos <- read_csv("data/external/events_mapping.csv") |> 
  filter(source == "IMOS") |> 
  mutate(keyword = tolower(keyword)) |> 
  unique()

imos_events <- imos_sf |> 
  filter(!is.na(MESO_NAME)) |> 
  unnest(cols = keywords) |> 
  mutate(keywords = tolower(keywords)) |> 
  inner_join(mapping_imos, join_by(keywords == keyword)) |> 
  select(NRI = source,
         datasetName = citation,
         datasetURI = metadata, 
         keywords, 
         year, 
         region = MESO_NAME,
         id, 
         label, 
         facet1,
         facet2, 
         facet3)
  
saveRDS(imos_events, "data/interim/imos_events.RDS")


### ALA ### -------

# still need to download records for 2022 - only 1 million + records???

ds <- open_dataset("data/galah") 

event_records <- ds |> 
  filter(year >= 2010,
         !is.na(cl1048) | !is.na(cl966),
         #!is.na(samplingProtocol),
         !is.na(eventID),
         !is.na(dataResourceUid)) |>
  mutate(samplingProtocol = tolower(samplingProtocol)) |> 
  group_by(decimalLatitude,
           decimalLongitude,
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
  collect()


# ALA spatial -----
imcra_sf <- read_sf("data/external/imcra4/imcra4_meso.shp") |> 
  st_transform(4326) |> 
  select(region = MESO_NAME)
  
ibra_sf <- read_sf("data/external/ibra7/ibra7_regions.shp") |> 
  st_transform(4326) |> 
  select(region = REG_NAME_7)

regions <- bind_rows(imcra_sf, ibra_sf)

sf_use_s2(FALSE)

ala_spatial <- event_records |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326) |> 
  st_join(regions, join = st_intersects) |> 
  st_set_geometry(NULL)

# summarise across events
event_summaries <- ala_spatial |> 
  group_by(dataResourceUid,
           dataResourceName, 
           samplingProtocol, 
           year, 
           region) |>
  summarise(eventsCount = n(),
            maxSpeciesCount = max(speciesCount),
            maxOccCount = max(occCount),
            minMinLeft = min(minLeft),
            maxMaxRight = max(maxRight)) |> 
  ungroup() |> 
  mutate(left_right_check = if_else(minMinLeft <= maxMaxRight, "correct", "wrong"))

# check left values less than or equal to right values 
# result should be 0
nrow(filter(event_summaries, left_right_check == "wrong"))

# ALA keywords --------
# get lft and rgt values for each kingdom
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
### ALTERNATIVELY 
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

kingdoms_lft_rgt <- readRDS("data/interim/kingdoms_lft_rgt.RDS")

events_filtered <- event_summaries |> 
  select(-left_right_check) |> 
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
  mutate(same = case_when(
    kingdom_left == kingdom_right ~ "same",
    TRUE ~ "different")) |> 
  mutate(remove = case_when(
    same == "different" & (kingdom_left == "Animalia" | kingdom_right == "Animalia") ~ "remove",
    TRUE ~ "keep")) |> 
  filter(remove == "keep") 


# events where more than 1 kingdom is sampled, and neither kingdom is 
# Animalia, get classified as plant events

events_filtered |> count(kingdom_left, kingdom_right, same, remove) |> View()
  
mapping_ala <- read_csv("data/external/events_mapping.csv") |>
  filter(str_detect(label, "Biodiversity")) |> 
  select(-source, -keyword) |> 
  unique()

ala_keywords <- search_taxa(mapping_ala$id)

# 4 rows with bie labels (haptophytes, coccolithophores, diatoms, bacteria) 
# don't produce a match using search_taxa()
ala_lft_rgt <- mapping_ala |>
  full_join(ala_keywords,
            join_by("id" == "search_term")) |> 
  select(id:facet3, lft, rgt) |> 
  rowwise() |> 
  mutate(levels = str_count(label, "\\|")) |> 
  ungroup()

events_classified <- events_filtered |>
  select(-remove) |> 
  mutate(classification = case_when(
    same == "different" ~ "Biodiversity | Plants",
    same == "same" & kingdom_left == "Plantae" ~ "Biodiversity | Plants",
    TRUE ~ "animal"))

# do joins separately for plants and animals
animal_events <- filter(events_classified, classification == "animal") 
plant_events <- filter(events_classified, classification != "animal")

joined_animal <- animal_events |> 
  select(-(kingdom_left:classification)) |> 
  left_join(ala_lft_rgt,
            join_by(within(minMinLeft, maxMaxRight, lft, rgt))) |> 
  group_by(dataResourceUid,
           dataResourceName, 
           samplingProtocol, 
           year,
           eventsCount, 
           maxSpeciesCount, 
           maxOccCount, 
           minMinLeft, 
           maxMaxRight) |>
  slice_max(levels, n = 1) |> 
  ungroup() |> 
  select(-c(minMinLeft, maxMaxRight, lft, rgt, levels))
  
joined_plants <- plant_events |> 
  left_join(ala_lft_rgt, join_by("classification" == "label")) |> 
  select(-c(minMinLeft, maxMaxRight, kingdom_left, kingdom_right, 
            same, lft, rgt, levels)) |> 
  rename(label = classification)
  
ala_events <- joined_animal |> 
  bind_rows(joined_plants) |> 
  mutate(datasetURI = paste0("https://collections.ala.org.au/public/show/", dataResourceUid),
         samplingProtocol = replace_na(samplingProtocol, "Protocol unspecified"),
         keywords = paste0(samplingProtocol, ": ", eventsCount, " samples"),
         NRI = "ALA") |> 
  select(NRI,
         datasetName = dataResourceName,
         datasetURI,
         keywords,
         year,
         region,
         id,
         label,
         facet1,
         facet2,
         facet3)

saveRDS(ala_events, "data/interim/ala_events.RDS")


### all events ### --------
tern_events <- readRDS("data/interim/tern_events.RDS")
imos_events <- readRDS("data/interim/imos_events.RDS")
ala_events <- readRDS("data/interim/ala_events.RDS")

events <- bind_rows(tern_events, imos_events, ala_events)
saveRDS(events, "data/processed/events.RDS")
write_csv(events, "data/processed/events.csv")
