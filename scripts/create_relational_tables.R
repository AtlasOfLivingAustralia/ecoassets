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
  write_parquet(sink = "data/interim/rel_distinct_taxa.parquet")

