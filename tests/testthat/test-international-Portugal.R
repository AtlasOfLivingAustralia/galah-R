# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Portugal works", {
  expect_message(galah_config(atlas = "Portugal"))
})

test_that("show_all(fields) works for Portugal", {
  skip_galah()
  x <- show_all(fields) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Portugal", {
  skip_galah()
  x <- show_all(collections, limit = 10) |>
    skip_unavailable()
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Portugal", {
  skip_galah()
  x <- show_all(datasets, limit = 10) |>
    skip_unavailable()
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Portugal", {
  skip_galah()
  x <- show_all(providers, limit = 10) |>
    skip_unavailable()
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Portugal", {
  skip_galah()
  x <- show_all(reasons) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Portugal", {
  skip_galah()
  x <- show_all(assertions) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Portugal", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for Portugal", {
  expect_error(show_all(lists))
})

test_that("search_all(fields) works for Portugal", {
  skip_galah()
  x <- search_all(fields, "year") |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Portugal", {
  skip_galah()
  x <- search_all(taxa, "Mammalia") |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Portugal", {
  skip_galah()
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  x <- search_all(fields, "basisOfRecord") |> 
    quiet_values() |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`atlas_counts()` works for Portugal", {
  skip_galah()
  x <- atlas_counts() |>
    dplyr::pull(count) |>
    skip_unavailable()
  expect_gt(x, 0)
})

test_that("`atlas_counts()` works with type = 'species' for Portugal", {
  skip_galah()
  x <- atlas_counts(type = "species") |>
    dplyr::pull(count) |>
    skip_unavailable()
  expect_gt(x, 0)
})

test_that("`count()` works with `identify()` for Portugal", {
  skip_galah()
  result <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect() |>
skip_unavailable()
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    filter(class == "Mammalia") |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})

test_that("`count()` works with `group_by()` for Portugal", {
  skip_galah()
  result <- galah_call() |>
    filter(year >= 2000) |>
    group_by(basisOfRecord) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_gt(nrow(result), 1)
  expect_equal(names(result),
               c("basisOfRecord", "basisOfRecord_label", "count"))
})

# NOTE: I've so far been unable to register for this atlas,
# so we can't test downloads. This affects `atlas_occurrences()` and 
# `atlas_species()`
test_that("`atlas_species()` returns error for Portugal", {
  skip_galah()
  galah_call(type = "species") |>
    filter(year == 2022) |>
    collect() |>
    expect_error()
})
test_that("`atlas_occurrences()` returns error for Portugal", {
  skip_galah()
  galah_call() |>
    filter(year == 1900) |>
    atlas_occurrences() |>
    expect_error()
})

# Note: atlas_media() should also work, in theory, but again am unable to test

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config)
