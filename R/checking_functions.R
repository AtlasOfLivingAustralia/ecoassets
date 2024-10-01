# Custom functions to check downloaded data for common errors

# safeguards against not noticing layers have been disabled
not_all_NA <- function(ds, col_name) {
  assert_that(has_name(ds, col_name))
  assert_that(
    sum(is.na(pull(ds, col_name, as_vector = TRUE))) < nrow(ds), 
    msg = "This column contains only NAs"
  )
}
