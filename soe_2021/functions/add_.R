# Sub-functions to support cross-tabulation by various categories

# group data by year
add_year_group <- function(df){
  var_factor <- cut(df$year, breaks = seq(1980, 2020, 5))
  levels(var_factor) <- paste(
    seq(1980, 2015, 5) + 1,
    seq(1985, 2020, 5),
    sep = "-")
  return(var_factor)
}


# by taxonomic group (pretty arbitrary categories)
add_taxon <- function(df){
  var <- rep(1, nrow(df)) # Other life forms
  var[which(df$kingdom == "Fungi")] <- 2 # "Fungi"
  var[which(df$kingdom == "Plantae")] <- 3 # "Plants"
  chordates <- df$phylum == "Chordata"
  var[(df$kingdom == "Animalia" & !chordates)] <- 4 # "Invertebrates"
  var[chordates] <- 9 # "Other Vertebrates"
  var[which(df$class == "Aves")] <- 5 # "Birds"
  var[which(df$class == "Mammalia")] <- 6 # "Mammals"
  var[which(df$class == "Reptilia")] <- 7 # "Reptiles"
  var[which(df$class == "Amphibia")] <- 8 #"Amphibians"

  # convert to factor
  var_factor <- factor(var,
    levels = seq_len(9),
    labels = c(
      "Other life forms",
      "Fungi",
      "Plants",
      "Invertebrates",
      "Birds",
      "Mammals",
      "Reptiles",
      "Amphibians",
      "Other vertebrates"))
  return(var_factor)
}


# Test add threat status
add_threat_status <- function(df) {
  # merge df with threatened df
  # add WONS list to EPBC list
  df$order <- seq_len(nrow(df))
  data_out <- merge(df,
                    all_threats_df[, c("taxon_concept_id",  "conservation_status")],
                    by.x = "species_guid",
                    by.y = "taxon_concept_id",
                    all.x = TRUE,
                    all.y = FALSE)

  data_out <- data_out[order(data_out$order), ] # NOTE: this hasn't been checked
  # convert raw data to a numeric variable
  status_vector <- rep(1, nrow(data_out))
  status_vector[data_out$conservation_status == "Conservation dependent"] <- 2
  status_vector[data_out$conservation_status == "Vulnerable"] <- 3
  status_vector[data_out$conservation_status == "Endangered"] <- 4
  status_vector[data_out$conservation_status == "Critically Endangered"] <- 5
  status_vector[data_out$conservation_status == "Extinct in the wild"] <- 6
  status_vector[data_out$conservation_status == "Extinct"] <- 7
  status_vector[data_out$conservation_status == "Weed"] <- 8
  # convert to factor
  status_factor <- factor(
    status_vector,
    levels = seq_len(8),
    labels = c(
      "Not threatened",
      "Conservation dependent",
      "Vulnerable",
      "Endangered",
      "Critically Endangered",
      "Extinct in the wild",
      "Extinct",
      "Weed of National Significance"))
  return(status_factor)
}


# aggregator function to group all relevant information at once.
# Called by crosstab_ala_data.table()
add_required_cols <- function(df, combns){
  if(any(combns == "year_group")){
    df$year_group <- add_year_group(df)
  }
  if(any(combns == "taxon")){
    df$taxon <- add_taxon(df)
  }
  if(any(combns == "threat_status")){
    df$threat_status <- add_threat_status(df)
  }
  if(any(combns == "griis_status")){
    df$griis_status <- add_griis_status(df)
  }
  return(df)
}
