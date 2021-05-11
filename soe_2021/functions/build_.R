## BUILD FUNCTIONS FOR ALA DATA
# take data from inst/extdata and save as a cleaned verison in /data

# first for EPBC
build_epbc_df <- function(){
  # classify by threatened status
  threatened_list <- read.csv("./SoE2021/inst/extdata/EPBC_list.csv")
  threatened_ala <- select_taxa(threatened_list$scientific_name)
  # threatened_ala[threatened_ala$match_type == "higherMatch", ]
  threatened_ala <- threatened_ala[!is.na(threatened_ala$species), ]
  threatened_df <- merge(threatened_list,
    threatened_ala[, c("search_term", "taxon_concept_id")],
    by.x = "scientific_name", by.y = "search_term",
    all = FALSE)

  # length(unique(threatened_list$taxon_concept_id)) == nrow(threatened_list)
  threatened_list <- split(threatened_df, threatened_df$taxon_concept_id)
  # threatened_list[which(unlist(lapply(threatened_list, nrow)) > 1)]
  threatened_df <- do.call(rbind, lapply(threatened_list, function(a){a[1, ]}))
  save(threatened_df, file = "./SoE2021/data/threatened_df.RData")
}


# Then for Weeds of National Significance (WONS)
build_wons_df <- function(){
  wons_df <- read.csv(
    "./SoE2021/inst/extdata/Weeds_of_National_Significance_(WoNS)_as_at_Feb._2013.csv")[,
    c("guid", "Supplied.Name")]
  colnames(wons_df) <- c("species_guid", "wons_supplied_name")
  wons_df$wons_included <- TRUE
  wons_df <- wons_df[wons_df$species_guid != "", ]
  save(wons_df, file = "./SoE2021/data/wons_df.RData")
}

# Weeds of national significance
build_threats_df <- function(epbc, wons) {
  wons$conservation_status <- "Weed"
  in_wons <- wons[, c("species_guid", "conservation_status")]
  colnames(in_wons)[1] <- "taxon_concept_id"
  all_threats_df <- rbind(in_wons, epbc[,c("taxon_concept_id", "conservation_status")])
  rownames(all_threats_df) <- all_threats_df$taxon_concept_id
  save(all_threats_df, file = "./SoE2021/data/all_threats_df.RData")
}

# ADD FUNCTIONS FOR ALA DATA
# For GRIIS list
# For GRIIS list
build_griis_df <- function(){
  griis_list <- read.csv(
    "./SoE2021/inst/extdata/GRIIS_Aus.csv")
  griis_ala_original <- select_taxa(griis_list$species)
  griis_ala <- griis_ala_original %>%
    filter(match_type == "exactMatch", issues == "noIssue")

  griis_ala <- griis_ala[!is.na(griis_ala$species), ]
  griis_df <- merge(griis_list,
                    griis_ala[, c("search_term", "taxon_concept_id")],
                    by.x = "species", by.y = "search_term",
                    all = FALSE)

  # length(unique(threatened_list$taxon_concept_id)) == nrow(threatened_list)
  griis_list <- split(griis_df, griis_df$taxon_concept_id)
  # threatened_list[which(unlist(lapply(threatened_list, nrow)) > 1)]
  griis_df <- do.call(rbind, lapply(griis_list, function(a){a[1, ]}))

  griis_df <- griis_df %>%
    mutate(isInvasive = ifelse(isInvasive == "NULL", "introduced", "invasive"))

  save(griis_df, file = "./SoE2021/data/griis_df.RData")
}
