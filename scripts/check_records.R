# Checks records for download errors

source("R/checking_functions.R")

ds <- open_dataset("data/galah")

check_col_count(ds, 33)

check_not_na(ds, speciesID)
check_not_na(ds, year)
check_not_na(ds, decimalLatitude)
check_not_na(ds, decimalLongitude)
