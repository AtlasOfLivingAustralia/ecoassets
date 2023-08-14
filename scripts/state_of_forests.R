
ds <- open_dataset("data/galah", format = "parquet")

dir.create("data/tmp_sof")

ds |> 
  filter(year >= 1970,
         cl22 != "",
         kingdom == "Plantae",
         cl10000 != "",
         scientificName != "",
         taxonRank %in% c("species", "variety", "subspecies", 
                          "cultivar", "subvariety", "form"),
         str_detect(speciesID, "https://")) |>
  select(speciesID,
         kingdom,
         phylum,
         class,
         order,
         family,
         genus,
         vernacularName,
         scientificName,
         decimalLatitude,
         decimalLongitude,
         cl22,
         cl10000,
         cl1048,
         cl966,
         taxonRank) |>
  write_dataset(path = "data/tmp_sof/plants", format = "parquet")

ds |> 
  filter(year >= 1970,
         cl22 != "",
         phylum == "Chordata",
         cl10000 != "",
         scientificName != "",
         taxonRank %in% c("species", "variety", "subspecies", 
                          "cultivar", "subvariety", "form"),
         str_detect(speciesID, "https://")) |>
  select(speciesID,
         kingdom,
         phylum,
         class,
         order,
         family,
         genus,
         vernacularName,
         scientificName,
         decimalLatitude,
         decimalLongitude,
         cl22,
         cl10000,
         cl1048,
         cl966,
         taxonRank) |>
  write_dataset(path = "data/tmp_sof/chordates", format = "parquet")




unlink("data/tmp_sof", recursive = TRUE)
