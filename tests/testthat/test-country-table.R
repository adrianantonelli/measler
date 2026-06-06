test_that("country_table returns a tibble for a real country", {
  Braziltibble <- country_table("Brazil")
  expect_s3_class(Braziltibble, "tbl_df")})

test_that("country_table returns the expected summary columns", {
  Countrytbl <- country_table("Brazil")
  expect_true(
    all(c("year", "total_cases", "avg_incidence_per_1M", "discarded_cases") %in% names(Countrytbl)))
})

test_that("country_table errors when data is not a data frame", {
  expect_error(country_table("Brazil", "not a data frame"), "`data` must be a data frame")
})

test_that("country_table errors on a numerical, non character country", {
  expect_error(country_table(5), "`selected_country` must be a single character string")
})

test_that("country_table errors when the country is not found", {
  expect_error(country_table("GreeNl@nd"), "not found in the data's `country` column")
})
