# Generates 3 datasets summarising monitoring effort across Australia using data 
# from the ALA, TERN, and IMOS. Events are intersections of location 
# (IBRA/IMCRA region), time (calendar year from 2010 onwards), and type of 
# monitoring (keywords mapped to labels and facets in 
# data/external/events_mapping.csv from DH, based on the GCMD vocabulary)


# spatial layers ------

imcra_sf <- read_sf(here("data", "external", "imcra4", "imcra4_meso.shp")) |> 
  st_transform(4326) |> 
  select(region = MESO_NAME)

ibra_sf <- read_sf(here("data", "external", "ibra7", "ibra7_regions.shp")) |> 
  st_transform(4326) |> 
  select(region = REG_NAME_7)

states_sf <- read_sf(here("data", "external", "aust_states", "aust_states.shp")) |> 
  st_transform(4326) |> 
  select(stateTerritory = STE_NAME21) |> 
  filter(stateTerritory != "Outside Australia") 

sf_use_s2(FALSE)


# ALA -------

ds <- open_dataset("data/galah")

# Events are sets of occurrence records where taxa have been identified to at 
# least species level, and have information about datasetID, sampling protocol, 
# location, and date. 

event_records <- ds |> 
  filter(year >= 2010,
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
  collect()

# Protocol records are grouped event records that meet a set of criteria, and 
# can be used as a filter to exclude unwanted event records downstream. Remove 
# protocol records if:
# 1. number of events < 10
# 2. max number of species per event < 5
# 3. min-min-left and max-max-right span more than 1 kingdom AND events 
#    include animal records (i.e. more likely to be an incidental collection 
#    of observations rather than directed monitoring effort)

kingdoms_lft_rgt <- readRDS(here("data", "interim", "kingdoms_lft_rgt.RDS"))

protocol_records <- event_records |> 
  group_by(dataResourceUid,
           dataResourceName,
           samplingProtocol, 
           eventID) |> 
  summarise(eventsCount = n(),
            maxSpeciesCount = max(speciesCount),
            maxOccCount = max(occCount),
            minMinLeft = min(minLeft),
            maxMaxRight = max(maxRight)) |> 
  ungroup() |> 
  col_vals_lte(columns = vars(minMinLeft), value = vars(maxMaxRight)) |> 
  filter(eventsCount >= 10,
         maxSpeciesCount >= 5) |> 
  left_join(kingdoms_lft_rgt,
            by = join_by(between(minMinLeft, lft, rgt, bounds = "[]"))) |> 
  select(-c(lft, rgt, rank)) |> 
  rename(kingdom_left = search_term) |> 
  left_join(kingdoms_lft_rgt, 
            by = join_by(between(maxMaxRight, lft, rgt, bounds = "[]"))) |>
  select(-c(lft, rgt, rank)) |> 
  rename(kingdom_right = search_term) |> 
  mutate(exclude = case_when(
    (kingdom_left != kingdom_right) &
      (kingdom_left == "Animalia" |
         kingdom_right == "Animalia") ~ "exclude",
    TRUE ~ "include")) |>
  filter(exclude == "include") |>
  select(-exclude)

### ALA keywords ------
mapping_ala <- read_csv(here("data", "external", "events_mapping.csv")) |>
  filter(str_detect(label, "Biodiversity")) |> 
  select(-source, -keyword) |> 
  unique()

# NOTE: NZOR ids (haptophytes, coccolithophores, diatoms, bacteria) don't 
# produce a match using search_taxa(); this is fine to ignore
ala_keywords <- search_taxa(mapping_ala$id)

ala_lft_rgt <- mapping_ala |>
  full_join(ala_keywords,
            by = join_by(id == search_term)) |> 
  select(id:facet3, lft, rgt) |> 
  rowwise() |> 
  mutate(levels = str_count(label, "\\|")) |> 
  ungroup()

# If events contain plant and non-plant (excluding animals) records, treat
# those as plant monitoring events
protocol_records_plant <- protocol_records |> 
  filter(kingdom_left == "Plantae" | kingdom_right == "Plantae") |> 
  mutate(label = "Biodiversity | Plants") |> 
  left_join(ala_lft_rgt, by = join_by(label))

protocol_records_animal <- protocol_records |> 
  filter(kingdom_left == "Animalia") |> 
  left_join(ala_lft_rgt,
            by = join_by(within(minMinLeft, maxMaxRight, lft, rgt))) |> 
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

protocol_records_mapped <- bind_rows(protocol_records_animal, 
                                     protocol_records_plant)

# Use mapped protocol records as a filter to remove event records that do not 
# meet the specified criteria
ala_events <- event_records |> 
  inner_join(protocol_records_mapped, 
             by = join_by(dataResourceUid, 
                          dataResourceName, 
                          samplingProtocol,
                          eventID),
             na_matches = "na",
             relationship = "many-to-many") |> 
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
         stateTerritory = case_when(!is.na(cl22) ~ cl22,
                                    is.na(cl22) & !is.na(stateTerritory) ~ stateTerritory,
                                    is.na(cl22) & is.na(stateTerritory) ~ NA)) |> 
  select(nri,
         datasetName = dataResourceName,
         datasetURI, 
         nriKeyword, 
         decimalLatitude,
         decimalLongitude,
         year, 
         stateTerritory,
         ibraRegion = cl1048,
         imcraRegion = cl966,
         featureID = id, 
         featureName = label, 
         featureFacet1 = facet1,
         featureFacet2 = facet2, 
         featureFacet3 = facet3) |> 
  distinct()

saveRDS(ala_events, "data/interim/events_ala.RDS") 

### OPTION 1: get lft rgt values -----
# This is the easier option for now
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
#
### OPTION 2: get lft rgt values -----
# needs to be re-run whenever names index changes
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


# TERN -----

# data was emailed as a csv from TERN
tern_csv <- read_csv(here("data", "external", "tern-20230323.csv"))

### TERN dates -------
# Event dates are each calendar year in the pipe-delimited visit_date column. 
# If an event lacks a visit date, then the date is every year from 
# commissioned_date onwards. 

year_end_tern = 2022

ongoing <- tern_csv |> 
  filter(!(is.na(visit_date) & is.na(date_commissioned))) |> 
  filter(is.na(visit_date) | visit_date == "ongoing") |> 
  mutate(year_start = year(dmy(date_commissioned)),
         year_end = year_end_tern) |> 
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
  bind_rows(ongoing) |> 
  select(-date_commissioned, -visit_date) |> 
  filter(year >= 2010)

### TERN spatial------
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
         stateTerritory,
         decimalLatitude = lat,
         decimalLongitude = long,
         features_of_interest,
         datasetURI = uri,
         datasetName) |>
  st_set_geometry(NULL)

### TERN keywords -----
mapping_tern <- read_csv(here("data", "external", "events_mapping.csv")) |> 
  filter(source == "TERN") |> 
  mutate(keyword = tolower(keyword)) |> 
  unique()

# Reclassify some keywords so they map to the provided options
tern_events <- tern_spatial |> 
  separate_longer_delim(features_of_interest, delim = " | ") |> 
  mutate(features_of_interest = tolower(features_of_interest),
         features_of_interest = case_when(
           features_of_interest == "fauna population" ~ "animal population",
           features_of_interest == "flux" ~ "climate",
           features_of_interest == "soil surface" ~ "soil horizon",
           TRUE ~ features_of_interest)) |> 
  left_join(mapping_tern, by = join_by(features_of_interest == keyword)) |> 
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
         featureFacet3 = facet3) |> 
  distinct()

saveRDS(tern_events, here("data", "interim", "events_tern.RDS"))


# IMOS ------

# To get raw data: 
# get records from the first page
# extract elements of interest for 1:n records in the first page
# get the resumption token for the next page
# get records from the second page
# continue until no further resumption tokens are available

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
            "keywords")

# easier to parse than an anonymous function within a nested purrr::map() 
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

saveRDS(imos_data, here("data", "interim", "events_imos_raw.RDS"))
# imos_data <- readRDS(here("data", "interim", "events_imos_raw.RDS"))

### IMOS dates ---------
year_end_imos = 2022

imos_dates <- imos_data |> 
  separate_wider_delim(start_period, 
                       delim = "T", 
                       names_sep = "", 
                       too_few = "align_start") |> 
  separate_wider_delim(end_period, 
                       delim = "T", 
                       names_sep = "", 
                       too_few = "align_start") |> 
  select(-start_period2, -end_period2) |> 
  mutate(start_year = year(parse_date_time(start_period1, c("ymd", "ym"))),
         end_year = year(parse_date_time(end_period1, c("ymd", "ym"))),
         inferred_end_year = case_when(
           is.na(end_year) ~ year_end_imos,
           !is.na(end_year) ~ end_year),
         inferred_dq_issue = case_when(
           inferred_end_year >= start_year ~ "correct",
           inferred_end_year < start_year ~ "error")) |> 
  filter(!is.na(start_year),
         inferred_dq_issue == "correct") |> 
  rownames_to_column() |>
  nest(data = c(start_year, inferred_end_year)) |> 
  mutate(year = map(data, ~ seq(.x$start_year, .x$inferred_end_year, by = 1))) |> 
  select(-c(data, 
            rowname, 
            end_year, 
            start_period1, 
            end_period1, 
            inferred_dq_issue)) |> 
  unnest(cols = year) |> 
  filter(year >= 2010)

### IMOS spatial -------
imos_spatial <- imos_dates |> 
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

### IMOS keywords ------
mapping_imos <- read_csv(here("data", "external", "events_mapping.csv")) |> 
  filter(source == "IMOS") |> 
  mutate(keyword = tolower(keyword)) |> 
  unique()

imos_events <- imos_spatial |> 
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
         stateTerritory,
         imcraRegion = region,
         featureID = id, 
         featureName = label, 
         featureFacet1 = facet1,
         featureFacet2 = facet2, 
         featureFacet3 = facet3) |> 
  distinct()

saveRDS(imos_events, here("data", "interim", "events_imos.RDS"))


# exported datasets ------
# tern_events <- readRDS("data/interim/events_tern.RDS")
# imos_events <- readRDS("data/interim/events_imos.RDS")
# ala_events <- readRDS("data/interim/events_ala.RDS")

ala_events |> 
  bind_rows(tern_events, imos_events) |> 
  write_csv(here("data", 
                 "aggregated_env_monitoring",
                 "aggregated_env_monitoring.csv"))

ala_events |> 
  bind_rows(tern_events) |> 
  filter(!is.na(ibraRegion)) |> 
  group_by(year,
           stateTerritory,
           ibraRegion,
           featureID,
           featureName,
           featureFacet1,
           featureFacet2,
           featureFacet3) |> 
  summarise(recordCount = n()) |> 
  write_csv(here("data", 
                 "summary_monitoring_effort_terrestrial",
                 "summary_monitoring_effort_terrestrial.csv"))

ala_events |> 
  bind_rows(imos_events) |> 
  filter(!is.na(imcraRegion)) |> 
  group_by(year,
           stateTerritory,
           imcraRegion,
           featureID,
           featureName,
           featureFacet1,
           featureFacet2,
           featureFacet3) |> 
  summarise(recordCount = n()) |> 
  write_csv(here("data", 
                 "summary_monitoring_effort_marine",
                 "summary_monitoring_effort_marine.csv"))
