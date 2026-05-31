test_that("monthly_table works properly", {
  tbl <- monthly_table(cases_month, "AMR")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("monthly_table works with selected_months", {
  tbl <- monthly_table(cases_month, "EUR", selected_months = c(1, 2, 3))
  expect_s3_class(tbl, "gt_tbl")
})

test_that("monthly_table errors on invalid region", {
  expect_error(monthly_table(cases_month, "XYZ"), "`selected_region` must be one of")
})

test_that("monthly_table errors on invalid months", {
  expect_error(monthly_table(cases_month, "AMR", selected_months = c(0, 13)), "`selected_months`")
})
