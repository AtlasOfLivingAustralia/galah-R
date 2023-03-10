context("Test international atlases: Estonia")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Estonia works", {
  expect_message(galah_config(atlas = "Estonia"))
})

test_that("show_all(fields) works for Estonia", {
  vcr::use_cassette("IA_Estonia_show_all_fields", {
    x <- show_all(fields)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Estonia", {
  vcr::use_cassette("IA_Estonia_show_all_collections", {
    x <- show_all(collections, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Estonia", {
  vcr::use_cassette("IA_Estonia_show_all_datasets", {
    x <- show_all(datasets, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Estonia", {
  vcr::use_cassette("IA_Estonia_show_all_providers", {
    x <- show_all(providers, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Estonia", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for Estonia", {
  vcr::use_cassette("IA_Estonia_show_all_assertions", {
    x <- show_all(assertions)
  })
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
  x <- search_all(fields, "year")
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_gte(nrow(x), 1) 
})

test_that("search_all(taxa) works for Estonia", {
  vcr::use_cassette("IA_Estonia_search_all_taxa", {
    x <- search_all(taxa, "Mammalia")
  })
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_equal(nrow(x), 1) 
})

vcr::use_cassette("IA_Estonia_show_values", {
  test_that("show_values works for Estonia", {
    x <- search_fields("basis_of_record") |> 
      show_values()
    expect_gt(nrow(x), 1)
  })
})

vcr::use_cassette("IA_Estonia_atlas_counts", {
  test_that("atlas_counts works for Estonia", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_Estonia_atlas_counts_group_by", {
  test_that("atlas_counts works with group_by for Estonia", {
    result <- galah_call() |>
      galah_filter(year >= 2018) |>
      galah_group_by(basis_of_record) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("basis_of_record", "count"))
  })
})

test_that("atlas_occurrences returns error for Estonia", {
  expect_error(atlas_occurrences(
    filter = galah_filter(year == 2020)
  ))
})

galah_config(atlas = "Australia")