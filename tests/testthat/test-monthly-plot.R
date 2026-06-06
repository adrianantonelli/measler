test_that("monthly_plot returns a plot for a possible region", {
  plot <- monthly_plot("AMR")
  expect_s3_class(plot, "ggplot")
})

test_that("monthly_plot works with months inputted for selected_months", {
  plot <- monthly_plot("EUR", selected_months = c(1, 2, 3))
  expect_s3_class(plot, "ggplot")
})

test_that("monthly_plot gives errors for invalid region", {
  expect_error(monthly_plot("XYZ"), "`selected_region` must be one of")
})

test_that("monthly_plot gives errors for invalid months", {
  expect_error(monthly_plot("AMR", selected_months = c(0, 13)), "`selected_months`")
})
