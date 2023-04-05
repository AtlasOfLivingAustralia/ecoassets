# separates downloaded records into 3 smaller relational tables & 
# appends EPBC and GRIIS values 

ds <- open_dataset("data/galah", format = "parquet")

# 01. distinct loc -------
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
  filter(!is.na(imcraRegion) | !is.na(ibraRegion)) |>
  distinct() |> 
  compute()

# reclassify forest and capad fields
# adding locationID makes relational joins easier

distinct_forest2018 <- loc |> 
  distinct(forest2018) |> 
  collect() 

distinct_forest2013 <- loc |> 
  distinct(forest2013) |> 
  collect() 

distinct_capad_t <- loc |> 
  distinct(capad_t_class) |> 
  collect() 

distinct_capad_m <- loc |> 
  distinct(capad_m_class) |> 
  collect() 

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
      capad_m_class != "IPA" & capad_t_class != "IPA" & (capad_m_class == "PA" | capad_t_class == "PA") ~ "PA",
      TRUE ~ "not protected")) |> 
  select(-c(capad_m_class, capad_t_class, forest2018, forest2013)) |> 
  collect() |> 
  rowid_to_column(var = "locationID") |> 
  write_parquet(sink = "data/interim/distinct_loc")


# 02. distinct taxon -------
# every unique species and associated taxonomic hierarchy 

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





