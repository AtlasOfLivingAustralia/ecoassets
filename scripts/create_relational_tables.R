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

# GRIIS version 1.10
distribution <- read_tsv(here("data", 
                              "external", 
                              "dwca-griis-australia-v1.10", 
                              "distribution.txt"))

species <- read_tsv(here("data",
                         "external",
                         "dwca-griis-australia-v1.10",
                         "speciesprofile.txt"))

taxon <- read_tsv(here("data",
                       "external",
                       "dwca-griis-australia-v1.10",
                       "taxon.txt"))

griis_list <- taxon |> 
  full_join(distribution, by = join_by(id)) |> 
  full_join(species, by = join_by(id))

# match names in GRIIS to names in ALA
search_taxa_griis <- griis_list |>
  select(kingdom, phylum, class, order, family, scientificName) |>
  group_split(scientificName) |> 
  map(\(df)
      df |>
        search_taxa()) |> 
  bind_rows() 

# remove duplicates and reclassify rows where the same species has > 1 GRIIS status
griis_list_matched <- search_taxa_griis |> 
  filter(match_type %in% c("exactMatch", "canonicalMatch"),
         issues == "noIssue",
         !is.na(species)) |> 
  select(search_term, scientific_name, taxon_concept_id) |> 
  mutate(search_term_last = str_split_i(search_term, "_", -1)) |>
  inner_join(griis_list,
            by = join_by(search_term_last == scientificName)) |> 
  select(scientific_name, isInvasive) |> 
  mutate(griisStatus = case_when(
    isInvasive == "Invasive"~ "Invasive",
    isInvasive == "Null" ~ "Introduced")) |> 
  select(-isInvasive) |> 
  distinct() |> 
  group_by(scientific_name) |> 
  mutate(count = n(),
         griisStatus = case_when(
           count > 1 & "Invasive" %in% (unique(griisStatus)) ~ "Invasive",
           TRUE ~ griisStatus)) |> 
  ungroup() |> 
  select(-count) |> 
  distinct() 
    
taxa |> 
  left_join(griis_list_matched, by = join_by(speciesName == scientific_name)) |> 
  collect() |> 
  mutate(griisStatus = replace_na(griisStatus, "Native")) |> 
  col_vals_not_null(columns = vars(speciesID, griisStatus, speciesName)) |> 
  write_parquet(sink = "data/interim/rel_distinct_taxa.parquet")


# occurrences ------
# grouped counts of speciesID, year, location, basisOfRecord, epbcStatus 

dir.create("data/tmp_loc")
dir.create("data/tmp_grp")
dir.create("data/tmp_epbc")

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
  left_join(locID, by = join_by(decimalLatitude, decimalLongitude)) |>
  select(-c(decimalLatitude, decimalLongitude)) |> 
  group_by(year) |> 
  write_dataset(path = "data/tmp_loc", format = "parquet")
  
# group_by and summarise for each year, then combine as a ds
# TODO: possibly move this function and the other summarising function in 
# facet_biodiversity.R to R/ and pass in the columns to group_by with 
# across(all_of()) and a vector - have not tested if this will work

pq_loc_files <- list.files("data/tmp_loc", full.names = TRUE, recursive = TRUE)

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
    write_parquet(sink = paste0("data/tmp_grp/year_", year_id, ".parquet"))
    
}

walk(pq_loc_files, count_by_year)

### checks -----
# 1. number of rows in ungrouped ds == sum of counts in grouped ds 
ds_loc <- open_dataset("data/tmp_loc", format = "parquet")

ds_grp <- open_dataset("data/tmp_grp", format = "parquet")
sum_counts_grp <- ds_grp |> 
  pull(counts, as_vector = TRUE) |> 
  sum()

are_equal(nrow(ds_loc), sum_counts_grp)

# EPBC list from DCCEEW 
# https://www.environment.gov.au/sprat-public/action/report
epbc_list <- read_csv(here("data", 
                           "external", 
                           "epbc_24092024.csv"),
                      skip = 1,
                      col_select = c(scientificName = `Scientific Name`, 
                                     epbc_status = `EPBC Threat Status`,
                                     kingdom = Kingdom,
                                     phylum = Phylum, 
                                     class = Class, 
                                     order = Order, 
                                     family = Family,
                                     genus = Genus))

# match names in EPBC to names in ALA
search_taxa_epbc <- epbc_list |>
  select(kingdom, phylum, class, order, family, genus, scientificName) |>
  group_split(scientificName) |> 
  map(\(df)
      df |>
        search_taxa()) |> 
  bind_rows() 

# remove duplicates and reclassify rows where the same species has > 1 EPBC status
epbc_list_matched <- search_taxa_epbc |> 
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) |> 
  select(search_term, scientific_name, taxon_concept_id) |> 
  mutate(search_term_last = str_split_i(search_term, "_", -1)) |> 
  inner_join(epbc_list, 
             by = join_by(search_term_last == scientificName)) |> 
  select(scientific_name, epbc_status) |> 
  distinct() |> 
  mutate(fct_threat = as_factor(epbc_status) |> 
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
  select(-c(count_species, epbc_status, fct_threat))

ds_grp |> 
  left_join(epbc_list_matched, 
            by = join_by(speciesName == scientific_name)) |> 
  mutate(epbcStatus = case_when(
    is.na(epbcStatus) ~ "Not listed",
    TRUE ~ epbcStatus)) |>
  select(-speciesName) |> 
  write_dataset(path = "data/tmp_epbc", format = "parquet")

### checks ----
# 1. number of rows in ungrouped ds == sum of counts in final grouped ds 
ds_epbc <- open_dataset("data/tmp_epbc", format = "parquet")
sum_counts_epbc <- ds_epbc |> 
  pull(counts, as_vector = TRUE) |> 
  sum()

are_equal(nrow(ds_loc), sum_counts_epbc)

# 2. number of rows before join == number of rows after join
are_equal(nrow(ds_grp), nrow(ds_epbc))

# write to disk and cleanup
open_dataset("data/tmp_epbc", format = "parquet") |> 
  write_parquet(sink = "data/interim/rel_occ_counts.parquet")

unlink("data/tmp_loc", recursive = TRUE)
unlink("data/tmp_grp", recursive = TRUE)
unlink("data/tmp_epbc", recursive = TRUE)
