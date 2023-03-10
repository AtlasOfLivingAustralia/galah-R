context("Test international atlases: Portugal")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Portugal works", {
  expect_message(galah_config(atlas = "Portugal"))
})

test_that("show_all(fields) works for Portugal", {
  vcr::use_cassette("IA_Portugal_show_all_fields", {
    x <- show_all(fields)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Portugal", {
  vcr::use_cassette("IA_Portugal_show_all_collections", {
    x <- show_all(collections, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Portugal", {
  vcr::use_cassette("IA_Portugal_show_all_datasets", {
    x <- show_all(datasets, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Portugal", {
  vcr::use_cassette("IA_Portugal_show_all_providers", {
    x <- show_all(providers, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Portugal", {
  vcr::use_cassette("IA_Portugal_show_all_reasons", {
    x <- show_all(reasons)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Portugal", {
  vcr::use_cassette("IA_Portugal_show_all_assertions", {
    x <- show_all(assertions)
  })
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
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Portugal", {
  vcr::use_cassette("IA_Portugal_search_all_taxa", {
    x <- search_all(taxa, "Mammalia")
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Portugal", {
  vcr::use_cassette("IA_Portugal_show_values_fields", {
    x <- search_all(fields, "basis_of_record") |> 
      show_values()
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

vcr::use_cassette("IA_Portugal_atlas_counts", {
  test_that("atlas_counts works for Portugal", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_Portugal_atlas_counts_identify", {
  test_that("atlas_counts works with galah_identify for Portugal", {
    result <- galah_call() |>
      galah_identify("Mammalia") |>
      atlas_counts()
    expect_gt(result$count, 1)
    
    result2 <- galah_call() |>
      galah_filter(class == "Mammalia") |>
      atlas_counts()
    
    expect_lt(
      sqrt((result2$count - result$count)^2) / result$count, 
      0.1) # i.e. <1% margin of error
  })
})

vcr::use_cassette("IA_Portugal_atlas_counts_group_by", {
  test_that("atlas_counts works with group_by for Portugal", {
    result <- galah_call() |>
      galah_filter(year >= 2000) |>
      galah_group_by(basis_of_record) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("basis_of_record", "count"))
  })
})

test_that("atlas_occurrences returns error for Portugal", {
  expect_error(atlas_occurrences(
    filter = galah_filter(year == 2020)
  ))
})

galah_config(atlas = "Australia")