context("Test international atlases: Sweden")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Sweden works", {
  expect_message(galah_config(atlas = "Sweden"))
})

test_that("show_all(collections) works for Sweden", {
  vcr::use_cassette("IA_Sweden_show_all_collections", {
    x <- show_all(collections, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Sweden", {
  vcr::use_cassette("IA_Sweden_show_all_datasets", {
    x <- show_all(datasets, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Sweden", {
  vcr::use_cassette("IA_Sweden_show_all_providers", {
    x <- show_all(providers, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for Sweden", {
  vcr::use_cassette("IA_Sweden_show_all_reasons", {
    x <- show_all(reasons)
  })
  expect_gte(nrow(x), 0) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Sweden", {
  vcr::use_cassette("IA_Sweden_show_all_assertions", {
    x <- show_all(assertions)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Sweden", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for Sweden", {
  expect_error(show_all(lists))
})

test_that("search_all(fields) works for Sweden", {
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Sweden", {
  vcr::use_cassette("IA_Sweden_search_all_taxa", {
    x <- search_all(taxa, "Mammalia")
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for Sweden", {
  vcr::use_cassette("IA_Sweden_show_values", {
    x <- search_fields("basisOfRecord") |> 
      show_values()
  })
  expect_gt(nrow(x), 1)
})

vcr::use_cassette("IA_Sweden_atlas_counts", {
 test_that("atlas_counts works for Sweden", {
   expect_gt(atlas_counts()$count, 0)
   expect_gt(atlas_counts(type = "species")$count, 0)
 })
})

# test_that("atlas_counts works with galah_identify for Sweden", {
#   vcr::use_cassette("IA_Sweden_identify_test_1", {
#     result <- galah_call() |>
#       galah_identify("Mammalia") |>
#       atlas_counts()
#   })
#   vcr::use_cassette("IA_Sweden_identify_test_2", {
#     result2 <- galah_call() |>
#       galah_filter(class == "Mammalia") |>
#       atlas_counts()
#   })
#   
#   expect_gt(result$count, 1)
#   expect_gt(result2$count, 1)
#   
#   # we expect a <1% margin of error
#   expect_true(
#     (sqrt((result2$count - result$count)^2) / result$count) < 0.1)
# })

vcr::use_cassette("IA_Sweden_atlas_counts_group_by", {
  test_that("atlas_counts works with group_by for Sweden", {
    skip_on_cran()
    result <- galah_call() |>
      galah_filter(year >= 2020) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})

test_that("atlas_occurrences works for Sweden", {
  skip_on_cran()
  galah_config(
    atlas = "Sweden",
    email = "martinjwestgate@gmail.com",
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year < 1850) |>
    galah_select(taxon_name, year) |>
    atlas_occurrences()
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
})

galah_config(atlas = "Australia")