# 2/3: distinct taxa 
# second of 3 scripts that separate downloaded records into 3 relational tables 
# every unique species and associated taxonomic hierarchy 
# appends GRIIS status to taxon list

ds <- open_dataset("data/galah", format = "parquet")

taxa <- ds |> 
  select(speciesID,
         kingdom, 
         phylum,
         class,
         order,
         family,
         genus,
         speciesName = species,
         cl1048,
         cl966) |> 
  filter(!is.na(cl966) | !is.na(cl1048)) |>
  select(-c(cl966, cl1048)) |> 
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
  inner_join(griis_list_dh, by = join_by(search_term == scientificName)) |> 
  select(scientific_name, isInvasive) |> 
  mutate(griisStatus = case_when(
    isInvasive == "Invasive"~ "Invasive",
    isInvasive == "Null" ~ "Introduced",
    TRUE ~ "Native")) |> 
  select(-isInvasive) 

taxa_with_griis <- taxa |> 
  left_join(griis_list, by = join_by(speciesName == scientific_name)) |> 
  compute()

# TODO: figure out how to automate and generalise this check
# look for records with multiple status types
# (needs to be run every time because name matching through galah can
# produce different results e.g. if the names index is updated)
# if multiple status types present, use hierarchy to determine which records to
# retain i.e. Invasive > Introduced > Native
duplicates <- taxa_with_griis |> 
  distinct() |> 
  group_by(speciesID) |> 
  summarise(count = n()) |> 
  filter(count > 1) |> 
  collect() 

taxa_with_griis |> 
  filter(speciesID == as.character(duplicates[1,1])) |> 
  collect() |> 
  View()

taxa_with_griis |> 
  distinct() |> 
  filter(!(speciesID == as.character(duplicates[1, 1]) & griisStatus == "Introduced")) |> 
  write_parquet(sink = "data/interim/distinct_taxa")
