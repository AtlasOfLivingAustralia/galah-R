test_that("`describe()` amends a query to type = 'occurrences-describe'", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    describe() |>
    capture()
  expect_equal(x$type, "data/occurrences-describe")
  expect_s3_class(x, c("query", "list"))
})

test_that("`describe()` ignores `count()`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    describe() |>
    count() |>
    capture()
  expect_equal(x$type, "data/occurrences-describe")
  expect_s3_class(x, c("query", "list"))
})

test_that("`describe()` returns all fields by default", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    describe() |>
    collect()
  expect_gt(nrow(x), 300)
  expect_s3_class(x, c("tbl", "tbl_df", "data.frame"))
})

test_that("`describe()` adheres to user-provided `select()`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    select(group = "basic") |>
    describe() |>
    collect()
  y <- default_columns()
  expect_true(all(x$name %in% y))
  expect_s3_class(x, c("tbl", "tbl_df", "data.frame"))
})

test_that("`describe()` works for `select(type = 'media')`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    select(group = "media") |>
    describe() |>
    collect()
  expect_gt(nrow(x), 2)
  expect_s3_class(x, c("tbl", "tbl_df", "data.frame"))
})