context("Test international atlases: France")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = France works", {
  expect_message(galah_config(atlas = "France"))
})

test_that("show_all(fields) works for France", {
  vcr::use_cassette("IA_France_show_all_fields", {
    x <- show_all(fields)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for France", {
  vcr::use_cassette("IA_France_show_all_collections", {
    x <- show_all(collections, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for France", {
  vcr::use_cassette("IA_France_show_all_datasets", {
    x <- show_all(datasets, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for France", {
  vcr::use_cassette("IA_France_show_all_providers", {
    x <- show_all(providers, limit = 10)
  })
  expect_lte(nrow(x), 10) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for France", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for France", {
  vcr::use_cassette("IA_France_show_all_assertions", {
    x <- show_all(assertions)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for France", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for France", {
  expect_error(show_all(lists))
})

test_that("search_all(fields) works for France", {
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for France", {
  vcr::use_cassette("IA_France_search_all_taxa", {
    x <- search_all(taxa, "Vulpes vulpes")
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for France", {
  vcr::use_cassette("IA_France_show_values", {
    x <- search_fields("basisOfRecord") |> 
      show_values()
  })
  expect_gt(nrow(x), 1)
})

vcr::use_cassette("IA_France_atlas_counts_records", {
  test_that("atlas_counts works for France", {
    expect_gt(atlas_counts()$count, 0)
  })
})

vcr::use_cassette("IA_France_atlas_counts_species", {
  test_that("atlas_counts works for France", {
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_France_atlas_counts_identify", {
  test_that("atlas_counts works with galah_identify for France", {
    skip_on_cran()
    result <- galah_call() |>
      galah_identify("Mammalia") |>
      atlas_counts()

    result2 <- galah_call() |>
      galah_filter(class == "Mammalia") |>
      atlas_counts()

    expect_lt(
      sqrt((result2$count - result$count)^2) / result$count,
      0.1) # i.e. <1% margin of error
  })
})

vcr::use_cassette("IA_France_atlas_counts_group_by", {
  test_that("atlas_counts works with group_by for France", {
    skip_on_cran()
    result <- galah_call() |>
      galah_filter(year >= 2018) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})


test_that("atlas_species works for France", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  x <- galah_call() |>
    galah_identify("Lagomorpha") |>
    atlas_species()
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_gt(nrow(x), 1)
  expect_gt(ncol(x), 1)
})


test_that("atlas_occurrences works for France", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  x <- galah_call() |>
    galah_filter(year <= 1950) |>
    galah_identify("Vulpes vulpes") |>
    galah_select(species, year) |>
    atlas_occurrences()
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_gt(nrow(x), 1)
  expect_equal(names(x), c("species", "year"))
})

galah_config(atlas = "Australia")