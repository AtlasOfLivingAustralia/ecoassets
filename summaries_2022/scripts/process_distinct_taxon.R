
# append GRIIS and EPBC lists to distinct_taxon file generated in AWS Athena
# distinct_taxon_griis: speciesId, kingdom, phylum, class, order, family, genus, 
# speciesName, griisStatus {introduced, invasive, native}
# Presence in the GRIIS list = introduced, having the Invasive flag = invasive,
# everything else (not on the list) = native

library(here)
library(data.table)
library(dplyr)
library(galah)

# from AWS
distinct_taxon <- fread(here("summaries_2022", 
                             "data", 
                             "raw", 
                             "aws",
                             "distinct_taxon_raw.csv"))

# GRIIS files from DH
distribution <- fread(here("summaries_2022", 
                           "data", 
                           "raw", 
                           "dwca-griis-australia-v1.6", 
                           "distribution.txt"))
species <- fread(here("summaries_2022", 
                      "data", 
                      "raw",
                      "dwca-griis-australia-v1.6", 
                      "speciesprofile.txt"))
taxa <- fread(here("summaries_2022", 
                   "data", 
                   "raw",
                   "dwca-griis-australia-v1.6", 
                   "taxon-edited.txt"), 
              fill = TRUE)

# create a GRIIS list
griis_list <- taxa %>% 
  full_join(distribution, by = "id") %>% 
  full_join(species, by = "id")

# look up ALA identifiers based on scientific names in GRIIS list
griis_ala_raw <- as.data.frame(search_taxa(griis_list$scientificName))

# clean up search_taxa() results
griis_ala_tidy <- griis_ala_raw %>% 
  filter(match_type == "exactMatch",
         issues == "noIssue",
         !is.na(species)) %>% 
  select(search_term, scientific_name, taxon_concept_id)

# add ALA identifiers to cleaned up GRIIS list 
# only includes species that match completely with ALA records, to species level etc.
griis <- inner_join(griis_list, 
                    griis_ala_tidy, 
                    by = c("scientificName" = "search_term"))

# join to distinct_taxon
distinct_taxon_griis <- griis %>% 
  select(scientific_name, isInvasive) %>% 
  right_join(distinct_taxon, by = c("scientific_name" = "speciesName")) %>% 
  mutate(griisStatus = case_when(
    isInvasive == "Invasive" ~ "Invasive",
    isInvasive == "Null" ~ "Introduced",
    is.na(isInvasive) ~ "Native")) %>% 
  select(-isInvasive) %>% 
  relocate(speciesName = scientific_name, .after = genus)

fwrite(distinct_taxon_griis, 
       here("summaries_2022", 
            "data", 
            "processed", 
            "distinct_taxon_griis.csv"))

