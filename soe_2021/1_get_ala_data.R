# function to get the raw data
# NOTE: this takes a VERY long time to run

library(galah)

# set config (note: ALA-registered email address needed)
ala_config(
  email = "email@email.com",
  cache_directory = "./data/raw_ALA",
  caching = TRUE,
  download_reason_id = 10,
  verbose = TRUE)

# set up list of states to iterate over
states <- sort(find_field_values("cl22")$category[1:8])
state_df <- data.frame(
  acronym = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
  full_name = states)

# in a loop, download and save data
invisible(lapply(
  split(state_df, seq_len(nrow(state_df)))[2:8],
  function(a){
    # ala_counts(filters = select_filters(cl22 = a$full_name))
    result <- ala_occurrences(
      # taxa = select_taxa(term = "Litoria peronii"), # testing
      filters = select_filters(
        year = c(1980:2021),
        cl22 = a$full_name),
      columns = select_columns(
        "species_guid", # species ID
        "year",
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "decimalLongitude",
        "decimalLatitude",
        "species",
        "basisOfRecord",
        "cl22", # states
        "cl1048", # IBRA
        "cl10944" # CAPAD 2016 (national park names)
      )
    )
    saveRDS(result,
      paste0("./raw_ALA/", a$acronym, ".rds"))
  }
))
