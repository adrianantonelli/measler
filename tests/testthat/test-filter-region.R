test_that("filter_region returns a tibble", {
  result <- filter_region(cases_month, "AMR")
  expect_s3_class(result, "tbl_df")
})

test_that("filter_region returns a tibble with correct column names", {
  result <- filter_region(cases_month, "AMR")
  expect_equal(names(result), c("month", "region", "avg_measles"))
})

test_that("filter_region filters correctly by region", {
  result <- filter_region(cases_month, "AMR")
  expect_equal(unique(result$region), "AMR")
})

test_that("filter_region works with valid months", {
  result <- filter_region(cases_month, "AMR", selected_months = c(1, 2, 3))
  expect_true(all(result$month %in% c(1, 2, 3)))
})

test_that("filter_region errors on invalid region", {
  expect_error(filter_region(cases_month, "XYZ"))
})

test_that("filter_region errors on invalid months", {
  expect_error(filter_region(cases_month, "AMR", selected_months = c(0, 13)))
})

