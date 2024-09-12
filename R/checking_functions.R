# Checks downloaded data for common errors
# TODO: add check to confirm no columns are entirely NULL/NA - 
# this should safeguard against layers being disabled 

check_col_count <- function(ds, col_count) {
   assert_that(
    length(ds$schema) == col_count,
    msg = "schema length does not match col_count"
  )
 }

check_not_na <- function(ds, col_name) {
   assert_that(
    sum(is.na(pull(ds, {{col_name}}, as_vector = FALSE))) == 0,
    msg = "this column contains one or more NAs"
  )
 }

# incomplete
check_not_all_na <- function(ds, col_name) {
  assert_that(
    all(is.na(pull(ds, {{col_name}}, as_vector = FALSE))) == 0,
    msg = "this column only contains NAs"
  )
}

