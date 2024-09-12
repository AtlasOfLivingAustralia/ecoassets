# Checks records for download errors

source("R/checking_functions.R")

ds <- open_dataset("data/galah")

check_col_count(33)

check_not_na(speciesID)
check_not_na(year)
check_not_na(decimalLatitude)
check_not_na(decimalLongitude)
