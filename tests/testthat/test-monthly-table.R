test_that("monthly_table returns a gt table for valid region", {
  tbl <- monthly_table("AMR")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("monthly_table works with selected_months", {
  tbl <- monthly_table("EUR", selected_months = c(1, 2, 3))
  expect_s3_class(tbl, "gt_tbl")
})

test_that("monthly_table errors on invalid region", {
  expect_error(monthly_table("XYZ"), "`selected_region` must be one of")
})

test_that("monthly_table errors on invalid months", {
  expect_error(monthly_table("AMR", selected_months = c(0, 13)), "`selected_months`")
})
