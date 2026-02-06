test_that("`glimpse()` returns the correct object class", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    filter(year == 2025) |>
    glimpse() |>
    collect()
  expect_s3_class(x, c("occurrences_glimpse", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3)
  # expect_equal(ncol(x), length(default_columns())) # FIXME: issue with ID/recordID
})