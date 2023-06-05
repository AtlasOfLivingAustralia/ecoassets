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
# TODO: look at implementing this check before joining (see similar check below
# in occurrences section)
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
dir.create("data/temp_grp")
dir.create("data/temp_epbc")
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
    write_parquet(sink = paste0("data/temp_grp/year_", year_id, ".parquet"))
    
}

walk(pq_loc_files, count_by_year)

### checks -----
# 1. number of rows in ungrouped ds == sum of counts in grouped ds 
ds_loc <- open_dataset("data/temp_loc", format = "parquet")
duck_loc <- ds_loc |> to_duckdb()

ds_grp <- open_dataset("data/temp_grp", format = "parquet")
sum_counts_grp <- ds_grp |> 
  pull(counts, as_vector = TRUE) |> 
  sum()

row_count_match(duck_loc, count = sum_counts_grp)

# EPBC list from Cam (via DAWE)
epbc_list_cam <- read_csv(here("data", 
                               "external", 
                               "epbc_20220503.csv"),
                          col_select = c(`Scientific Name`, 
                                         `Threatened status`))

# create EPBC list that exactly matches names in ALA, reclassify, join
search_taxa_epbc <- search_taxa(epbc_list_cam$`Scientific Name`)

# remove duplicates and reclassify rows where the same species has > 1 EPBC status
epbc_list <- search_taxa_epbc |> 
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) |> 
  select(search_term, scientific_name, taxon_concept_id) |> 
  inner_join(epbc_list_cam, 
             by = join_by(search_term == `Scientific Name`)) |> 
  select(scientific_name, `Threatened status`) |> 
  distinct() |> 
  mutate(fct_threat = as_factor(`Threatened status`) |> 
           fct_relevel("Extinct",
                       "Extinct in the wild",
                       "Critically Endangered",
                       "Endangered",
                       "Vulnerable",
                       "Conservation Dependent")) |> 
  group_by(scientific_name) |> 
  mutate(count_species = n()) |> 
  # works because factors are coded in increasing numeric values (e.g. extinct = 1)
  slice_min(fct_threat) |> 
  ungroup() |> 
  mutate(epbcStatus = as.character(fct_threat)) |> 
  select(-c(count_species, `Threatened status`, fct_threat))

ds_grp |> 
  left_join(epbc_list, 
            by = join_by(speciesName == scientific_name)) |> 
  mutate(epbcStatus = case_when(
    is.na(epbcStatus) ~ "Not listed",
    TRUE ~ epbcStatus)) |> 
  select(-speciesName) |> 
  write_dataset(path = "data/temp_epbc", format = "parquet")

### checks ----
# 1. number of rows in ungrouped ds == sum of counts in final grouped ds 
ds_epbc <- open_dataset("data/temp_epbc", format = "parquet")
sum_counts_epbc <- ds_epbc |> 
  pull(counts, as_vector = TRUE) |> 
  sum()

row_count_match(duck_loc, count = sum_counts_epbc)

# 2. number of rows before join == number of rows after join
nrow_grp <- ds_grp |> nrow()
duck_epbc <- ds_epbc |> to_duckdb()

row_count_match(duck_epbc, count = nrow_grp)

# write to disk and cleanup
open_dataset("data/temp_epbc", format = "parquet") |> 
  write_parquet(sink = "data/interim/rel_occ_counts.parquet")

unlink("data/temp_loc", recursive = TRUE)
unlink("data/temp_grp", recursive = TRUE)
unlink("data/temp_epbc", recursive = TRUE)
dbDisconnect(duck_con, shutdown=TRUE)
