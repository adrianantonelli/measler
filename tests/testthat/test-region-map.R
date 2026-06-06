test_that("region_map returns a ggplot for a valid region", {
  AmrMap <- region_map("AMR")
  expect_s3_class(AmrMap, "ggplot")
})

test_that("region_map errors when data is not a data frame", {
  expect_error(region_map("AMR", "not a data frame"), "`data` must be a data frame")
})

test_that("region_map errors on an invalid region", {
  expect_error(region_map("XYZ"), "`selected_region` must be one of")
})
