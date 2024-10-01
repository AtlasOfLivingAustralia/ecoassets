# Checks records for download errors

source("R/checking_functions.R")

ds <- open_dataset("data/galah")

# 1. checks number of downloaded columns
assert_that(are_equal(ncol(ds), 33))

# 2. checks columns for NAs
assert_that(noNA(pull(ds, year, as_vector = TRUE)))
assert_that(noNA(pull(ds, decimalLatitude, as_vector = TRUE)))
assert_that(noNA(pull(ds, decimalLongitude, as_vector = TRUE)))
assert_that(noNA(pull(ds, speciesID, as_vector = TRUE)))

# 3. checks none of the columns contain only NAs
# TODO: loop this over all columns
assert_that(not_all_NA(ds, "cl10000"))
