test_that("month-f-test produces a gt_tbl object", {

  table1 <- month_f_test("AFR")
  table2 <- month_f_test(c("AFR", "AMR"))
  table3 <- month_f_test("AFR", 2:5)
  table4 <- month_f_test("SEAR", c(1,5,7))
  table5 <- month_f_test(c("AFR", "SEAR", "AMR"), c(1,6,12))

  expect_s3_class(table1, "gt_tbl")
  expect_s3_class(table2, "gt_tbl")
  expect_s3_class(table3, "gt_tbl")
  expect_s3_class(table4, "gt_tbl")
  expect_s3_class(table5, "gt_tbl")
})


test_that("month_f_test returns one row for one region", {
  result <- month_f_test("AMR")
  body <- gt::extract_body(result)

  expect_equal(nrow(body), 1)
  expect_equal(body$Region, "Americas")
  expect_equal(body$Region_Code, "AMR")
})

test_that("month_f_test returns one row per selected region", {
  result <- month_f_test(c("AMR", "AFR"))
  body <- gt::extract_body(result)

  expect_equal(nrow(body), 2)
  expect_true(all(c("AMR", "AFR") %in% body$Region_Code))
})

test_that("month_f_test errors for invalid regions", {
  expect_error(month_f_test("XYZ"),
               "`selected_regions` must be from")
})

test_that("month_f_test errors for invalid months", {
  expect_error(month_f_test("AMR", selected_months = 13),
               "`selected_months` must only contain month numbers from 1 to 12")
})
