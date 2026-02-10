quiet_print <- purrr::quietly(print.occurrences_glimpse)

test_that("`glimpse()` returns the correct object class", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    filter(year == 2025) |>
    glimpse() |>
    collect()
  expect_s3_class(x, c("occurrences_glimpse", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3) # number of rows in the tibble
  x_print <- strsplit(quiet_print(x)$messages, "\n")[[1]] # print statement
  expect_equal(length(x_print), 10)
  # expect_equal(ncol(x), length(default_columns())) # FIXME: issue with ID/recordID
})

test_that("`glimpse()` works with `select()`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    filter(year == 2025) |>
    select(eventDate, decimalLatitude, year) |>
    glimpse() |>
    collect()
  expect_s3_class(x, c("occurrences_glimpse", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3)
  x_print <- strsplit(quiet_print(x)$messages, "\n")[[1]]
  expect_equal(length(x_print), 5)
  # ensure all three fields are present
  stringr::str_detect(x_print,
                      "^\\$ (eventDate|decimalLatitude|year)") |>
    which() |>
    length() |>
    expect_equal(3)
})

rm(quiet_print)