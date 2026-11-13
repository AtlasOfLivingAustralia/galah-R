test_that("`describe()` amends a query to type = 'occurrences-describe'", {
  skip_galah()
  x <- galah_call() |>
    describe() |>
    capture()
  expect_equal(x$type, "data/occurrences-describe")
  expect_s3_class(x, c("query", "list"))
})

test_that("`describe()` ignores `count()`", {
  skip_galah()
  x <- galah_call() |>
    describe() |>
    count() |>
    capture()
  expect_equal(x$type, "data/occurrences-describe")
  expect_s3_class(x, c("query", "list"))
})

test_that("`describe()` returns all fields by default", {
  skip_galah()
  x <- galah_call() |>
    describe() |>
    collect()
  expect_gt(nrow(x), 300)
  expect_tibble(x)
})

test_that("`describe()` adheres to user-provided `select()`", {
  skip_galah()
  x <- galah_call() |>
    select(group = "basic") |>
    describe() |>
    collect()
  y <- default_columns()
  expect_tibble(x)
  expect_true(all(x$id %in% y))
})

test_that("`describe()` works for `select(type = 'media')`", {
  skip_galah()
  x <- galah_call() |>
    select(group = "media") |>
    describe() |>
    collect()
  expect_tibble(x)
  expect_gt(nrow(x), 2)
})