test_that("`filter()` works for a single 'equals' argument", {  
  filters <- galah_filter(year == 2010)
  expect_s3_class(filters, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 1)
})

test_that("`filter()` gives an error for single equals sign", {
  expect_error(galah_filter(year = 2010))
})

test_that("`filter()` returns empty tibble when no arguments specified", {
  filters <- galah_filter()
  expect_s3_class(filters, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 0)
})
