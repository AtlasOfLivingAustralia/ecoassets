# Separates downloaded occurrence records into three relational tables to
# simplify aggregating and faceting prior to generating biodiversity datasets

ds <- open_dataset("data/galah", format = "parquet")

# location ----
# every unique lat/lon combination and associated spatial values 
# (CAPAD, IBRA, IMCRA, state/territory, Forests of Australia)

loc <- ds |> 
  select(decimalLatitude,
         decimalLongitude,
         stateTerritory = cl22,
         capad_m_class = cl111032,
         capad_t_class = cl111033,
         forest2018 = cl10000,
         forest2013 = cl10902,
         ibraRegion = cl1048,
         imcraRegion = cl966) |> 
  distinct() |> 
  compute()

# distinct_forest2018 <- loc |> 
#   distinct(forest2018) |> 
#   collect() 
# 
# distinct_forest2013 <- loc |> 
#   distinct(forest2013) |> 
#   collect() 
# 
# distinct_capad_t <- loc |> 
#   distinct(capad_t_class) |> 
#   collect() 
# 
# distinct_capad_m <- loc |> 
#   distinct(capad_m_class) |> 
#   collect() 

# reclassify values in forest and capad fields, and add ID to simplify joins
loc |> 
  mutate(
    forest2018Status = case_when(
      forest2018 == "Non forest" | is.na(forest2018) ~ "non-forest",
      TRUE ~ "forest"),
    forest2013Status = case_when(
      forest2013 == "Non Forest" | is.na(forest2013) ~ "non-forest",
      TRUE ~ "forest"),
    capad_m_class = case_when(
      capad_m_class == "Indigenous Protected Area" ~ "IPA",
      is.na(capad_m_class) ~ "not protected",
      TRUE ~ "PA"), 
    capad_t_class = case_when(
      capad_t_class == "Indigenous Protected Area" | capad_t_class == "Aboriginal Area" ~ "IPA",
      is.na(capad_t_class) ~ "not protected",
      TRUE ~ "PA"),
    capadStatus = case_when(
      capad_m_class == "IPA" | capad_t_class == "IPA" ~ "IPA",
      capad_m_class == "PA" | capad_t_class == "PA" ~ "PA",
      TRUE ~ "not protected")) |> 
  select(-c(capad_m_class, capad_t_class, forest2018, forest2013)) |> 
  collect() |> 
  rowid_to_column(var = "locationID") |> 
  write_parquet(sink = "data/interim/rel_distinct_loc.parquet")


# taxon ------
# every unique species and associated taxonomic ranks and GRIIS status 

taxa <- ds |> 
  select(speciesID,
         kingdom, 
         phylum,
         class,
         order,
         family,
         genus,
         speciesName = species) |> 
  distinct() |> 
  compute()

# modified GRIIS files from DH
distribution <- read_tsv(here("data", 
                              "external", 
                              "dwca-griis-australia-v1.6", 
                              "distribution.txt"))

species <- read_tsv(here("data",
                         "external",
                         "dwca-griis-australia-v1.6",
                         "speciesprofile.txt"))

taxon_edited <- read_tsv(here("data",
                              "external",
                              "dwca-griis-australia-v1.6",
                              "taxon-edited.txt"))

griis_list_dh <- taxon_edited |> 
  full_join(distribution, by = join_by(id)) |> 
  full_join(species, by = join_by(id))

# create GRIIS list that matches names in ALA, reclassify, join
search_taxa_griis <- search_taxa(griis_list_dh$scientificName)

griis_list <- search_taxa_griis |> 
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) |> 
  select(search_term, scientific_name, taxon_concept_id) |> 
  inner_join(griis_list_dh, 
             by = join_by(search_term == scientificName),
             relationship = "many-to-many") |> 
  select(scientific_name, isInvasive) |> 
  mutate(griisStatus = case_when(
    isInvasive == "Invasive"~ "Invasive",
    isInvasive == "Null" ~ "Introduced",
    TRUE ~ "Native")) |> 
  select(-isInvasive) 

taxa_with_griis <- taxa |> 
  left_join(griis_list, by = join_by(speciesName == scientific_name)) |>
  distinct() |> 
  collect() |> 
  mutate(griisStatus = replace_na(griisStatus, "Native")) 

# sometimes rows are duplicated, with the only difference being the griis status
# for the same species
# this filters out the less severe griis status in such instances
taxa_griis_counts <- taxa_with_griis |> 
  count(speciesID) |> 
  full_join(taxa_with_griis, by = join_by(speciesID))

taxa_griis_counts |> 
  group_by(speciesID) |> 
  mutate(griisStatus = case_when(
    `n` > 1 & "Invasive" %in% (unique(griisStatus)) ~ "Invasive",
    `n` > 1 & "Introduced" %in% (unique(griisStatus)) ~ "Introduced",
    TRUE ~ griisStatus)) |> 
  select(-`n`) |> 
  distinct() |>
  col_vals_not_null(columns = vars(speciesID, griisStatus)) |> 
  write_parquet(sink = "data/interim/rel_distinct_taxa.parquet")


# occurrences ------
# grouped counts of speciesID, year, location, basisOfRecord, epbcStatus 

dir.create("data/temp_loc")
dir.create("data/temp_grp_01")
dir.create("data/temp_grp_02")
dir.create("data/temp_distinct")
# because pointblank doesn't support arrow ds yet
duck_con <- dbConnect(duckdb::duckdb())

# substitute lat/lon with locationID and partition by year to 
# avoid running out of memory when grouping and counting
locID <- read_parquet("data/interim/rel_distinct_loc.parquet", 
                      col_select = c(locationID, 
                                     decimalLatitude, 
                                     decimalLongitude))

ds |> 
  select(speciesID,
         speciesName = species, 
         year,
         decimalLatitude,
         decimalLongitude,
         basisOfRecord) |> 
  mutate(basisOfRecord = tolower(basisOfRecord)) |> 
  left_join(locID, by = join_by(decimalLatitude, decimalLongitude)) |>
  select(-c(decimalLatitude, decimalLongitude)) |> 
  group_by(year) |> 
  write_dataset(path = "data/temp_loc", format = "parquet")
  
# group_by and summarise for each year, then combine as a ds
pq_loc_files <- list.files("data/temp_loc", full.names = TRUE, recursive = TRUE)

count_by_year <- function(fpath) {
  
  year_id <- sub('.*=(.*?)/.*', '\\1', fpath)
  
  fpath |> 
    read_parquet() |> 
    group_by(speciesID,
          speciesName,
          locationID,
          basisOfRecord) |> 
    summarise(counts = n(), .groups = "drop") |>
    mutate(year = year_id) |> 
    write_parquet(sink = paste0("data/temp_grp_01/year_", year_id, ".parquet"))
    
}

walk(pq_loc_files, count_by_year)

# check that the total number of rows in the ungrouped ds match the 
# summed counts in the grouped ds 
ds_loc <- open_dataset("data/temp_loc", format = "parquet")
duck_loc <- ds_loc |> to_duckdb()

ds_grp <- open_dataset("data/temp_grp_01", format = "parquet")

summed_counts_01 <- ds_grp |> 
  pull(counts, as_vector = TRUE) |> 
  sum()

row_count_match(duck_loc, count = summed_counts_01)

# EPBC list from Cam (via DAWE)
epbc_list_cam <- read_csv(here("data", "external", "epbc_20220503.csv"),
                          col_select = c(`Scientific Name`, 
                                         `Threatened status`))

# create EPBC list that matches names in ALA, reclassify, join
search_taxa_epbc <- search_taxa(epbc_list_cam$`Scientific Name`)

epbc_list <- search_taxa_epbc |> 
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) |> 
  select(search_term, scientific_name, taxon_concept_id) |> 
  inner_join(epbc_list_cam, 
             by = join_by(search_term == `Scientific Name`)) |> 
  select(scientific_name, `Threatened status`) |> 
  mutate(epbcStatus = case_when(
    is.na(`Threatened status`) ~ "Not listed",
    TRUE ~ as.character(`Threatened status`))) 

# same approach as above to remove distinct records for every year:
# essentially split-apply-combine but using datasets
ds_grp |> 
  left_join(epbc_list, by = join_by(speciesName == scientific_name)) |> 
  select(-c(speciesName, `Threatened status`)) |> 
  mutate(epbcStatus = case_when(
    is.na(epbcStatus) ~ "Not listed",
    TRUE ~ epbcStatus)) |> 
  group_by(year) |> 
  write_dataset(path = "data/temp_grp_02", format = "parquet")

pq_epbc_files <- list.files("data/temp_grp_02", full.names = TRUE, recursive = TRUE)

get_distinct <- function(fpath) {
  
  year_id <- sub('.*=(.*?)/.*', '\\1', fpath)
  
  fpath |> 
    read_parquet() |> 
    distinct() |> 
    mutate(year = year_id) |> 
    write_parquet(sink = paste0("data/temp_distinct/year_", year_id, ".parquet"))
  
 }

walk(pq_epbc_files, get_distinct)

# check that the total number of rows in the original ds match the 
# summed counts in the ds after running distinct() 
ds_distinct <- open_dataset("data/temp_distinct", format = "parquet")

summed_counts_02 <- ds_distinct |> 
  pull(counts, as_vector = TRUE) |> 
  sum()

row_count_match(duck_loc, count = summed_counts_02)

open_dataset("data/temp_distinct") |> 
  write_parquet(sink = "data/interim/rel_occ_counts.parquet")

unlink("data/temp_loc", recursive = TRUE)
unlink("data/temp_grp_01", recursive = TRUE)
unlink("data/temp_grp_02", recursive = TRUE)
unlink("data/temp_distinct", recursive = TRUE)
dbDisconnect(duck_con, shutdown=TRUE)




### Checks not working - number of rows after running distinct >= original ds
### TODO: implement something similar to what was done with griis list, where 
# epbc status is reclassified after checking for duplicates

check_files <- list.files("data/temp_distinct", full.names = TRUE, recursive = TRUE)
newfun <- function(fpath) {
  
  x <- fpath |> 
    read_parquet() |> 
    count(speciesID, locationID, basisOfRecord, counts, year) |> 
    filter(`n` > 1) |> 
    nrow()
  
}

y <- map(check_files, newfun)
# this value is still <= the difference in counts... still missing smth
sum(unlist(y))
