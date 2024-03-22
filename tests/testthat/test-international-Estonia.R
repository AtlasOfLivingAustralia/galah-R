# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Estonia works", {
  expect_message(galah_config(atlas = "Estonia"))
})

test_that("show_all(fields) works for Estonia", {
  skip_if_offline()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Estonia", {
  skip_if_offline()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Estonia", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Estonia", {
  skip_if_offline()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Estonia", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for Estonia", {
  skip_if_offline()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Estonia", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for Estonia", {
  expect_error(show_all(lists))
})

test_that("search_all(fields) works for Estonia", {
  skip_if_offline()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_gte(nrow(x), 1) 
})

test_that("search_all(taxa) works for Estonia", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_equal(nrow(x), 1) 
})

test_that("show_values works for Estonia", {
  skip_if_offline()
  x <- search_fields("basis_of_record") |> 
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
})

test_that("atlas_counts works for Estonia", {
  skip_if_offline()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Estonia", {
  skip_if_offline()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with group_by for Estonia", {
  skip_if_offline()
  result <- galah_call() |>
    galah_filter(year >= 2018) |>
    galah_group_by(basis_of_record) |>
    atlas_counts() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("basis_of_record", "count"))
})

test_that("atlas_counts errors when too many fields passed to group_by for Estonia", {
  skip_if_offline()
  expect_error(
    result <- galah_call() |>
      galah_filter(year >= 2018) |>
      galah_group_by(basis_of_record, year) |>
      atlas_counts(),
    "Too many fields"
  )
})

test_that("atlas_species returns error for Estonia", {
  expect_error({galah_call(type = "species") |>
      identify("Carnivora") |>
      collect()
  })
})

test_that("atlas_occurrences returns error for Estonia", {
  expect_error(atlas_occurrences(
    filter = galah_filter(year == 2020)
  ))
})

galah_config(atlas = "Australia")