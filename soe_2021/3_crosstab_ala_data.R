# script to run cross-tabulations of ALA data for the SoE 2021 app

# load required packages and functions
library(data.table)
source("./functions/add_.R")
source("./functions/build_.R")


# set up national park information
national_park_logical <- data_in$CAPAD2016Terrestrial != ""
national_park_numeric <- as.numeric(national_park_logical) + 1
national_park_factor <- factor(
  national_park_numeric,
  levels = seq_len(2),
  labels = c("Other land use", "National Park"))
data_in$national_parks <- national_park_factor

# work out required combinations of variables
crosstab_columns <- c(
  "year_group",
  "taxon",
  "basisOfRecord",
  "threat_status",
  "australianStatesAndTerritories",
  "iBRA7Regions",
  "national_parks",
  "griis_status")

# get all combinations
combination_list <- do.call(c,
  lapply(
    seq_len(4), # maximum number of combinations
    function(a){combn(crosstab_columns, a, simplify = FALSE)}))

# we never need to combine states and IBRA regions - remove pairs with these attributes
combination_list <- combination_list[
  !unlist(lapply(combination_list, function(a){
    all(
      c("australianStatesAndTerritories", "iBRA7Regions") %in% a)
  }))]


# now create list of combinations that we can populate with data
data_list <- as.list(rep(NA, length(combination_list)))
exceptions_group <- c("year_group", "threat_status", "taxon")

for(a in seq_along(data_list)){
  combn_tr <- combination_list[[a]]
  if(any(combn_tr %in% exceptions_group)){
    included_vars <- combn_tr[!(combn_tr %in% exceptions_group)]
    if(any(combn_tr == "year_group")){
      included_vars <- c(included_vars, "year")
    }
    if(any(combn_tr %in% c("threat_status", "griis_status"))){
      included_vars <- c(included_vars, "species_guid")
    }
    if(any(combn_tr == "taxon")){
      included_vars <- c(included_vars, "kingdom", "phylum", "class")
    }
  }else{
    included_vars <- combn_tr
  }

  # use included_vars to crosstab
  xtab1 <- add_required_cols(
    data_in[, .(.N), keyby = included_vars],
    combn_tr)
  xtab_final <- xtab1[, .(sum(N)), keyby = combn_tr]
  colnames(xtab_final)[length(combn_tr) + 1] <- "n_records"

  # get information by species
  if(any(included_vars == "species_guid")){
    xtab_species <- xtab1
  }else{
    xtab_species <- add_required_cols(
      data_in[, .(.N), keyby = c(included_vars, "species_guid")],
      combn_tr)
  }
  xtab_species_count <- xtab_species[
    (species_guid != ""),
    .(.N),
    keyby = combn_tr]

  colnames(xtab_species_count)[length(combn_tr) + 1] <- "n_species"
  result <- merge(xtab_final, xtab_species_count)
  data_list[[a]] <- result
  cat(paste0("Run ", a, " complete: ", Sys.time(), "\n"))

} # end loop

names(data_list) <- unlist(lapply(
  combination_list,
  function(a){paste(a, collapse = "::")}))

# last stage is to export xtab list and a corresponding index data.frame
save(data_list, file = "./SoE2021/data/data_list.RData")
