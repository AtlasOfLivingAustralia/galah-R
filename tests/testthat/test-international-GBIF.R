context("Test international atlases: GBIF")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = GBIF works", {
  expect_message(galah_config(atlas = "GBIF",
                              username = "atlasoflivingaustralia",
                              email = "ala4r@ala.org.au",
                              password = "galah-gbif-test-login"))
})

test_that("show_all(fields) works for GBIF", {
  x <- show_all_fields()
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

# vcr::use_cassette("IA_GBIF_show_all_collections", {
test_that("show_all(collections) works for GBIF", {
  x <- show_all(collections, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "collections") |>
    slice_head(n = 10) |>
    collect()
  expect_equal(nrow(y), 10)
  expect_true(inherits(y, c("tbl_df", "tbl", "data.frame")))
})

# vcr::use_cassette("IA_GBIF_show_all_datasets", {
test_that("show_all(datasets) works for GBIF", {
  x <- show_all(datasets, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

# vcr::use_cassette("IA_GBIF_show_all_providers", {
test_that("show_all(providers) works for GBIF", {
  x <- show_all(providers, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for GBIF", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for GBIF", {
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for GBIF", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for GBIF", {
  expect_error(show_all(lists))
})

# vcr::use_cassette("IA_GBIF_search_all_taxa", {
test_that("search_all(taxa) works for GBIF", {
  expect_equal(nrow(search_all(taxa, "Mammalia")), 1)
})

galah_config(verbose = TRUE)

# vcr::use_cassette("IA_GBIF_search_all_datasets", {
test_that("search_all(datasets) works for GBIF", {
  x <- expect_message(search_all(datasets, "Mammals"))
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

# vcr::use_cassette("IA_GBIF_search_all_collections", {
test_that("search_all(collections) works for GBIF", {
  x <- expect_message(search_all(collections, "Museum"))
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

# vcr::use_cassette("IA_GBIF_search_all_providers", {
test_that("search_all(providers) works for GBIF", {
  x <- expect_message(search_all(providers, "Frog"))
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

galah_config(verbose = FALSE)

test_that("search_all(fields) works for GBIF", {
  result <- search_all(fields, "year")
  expect_equal(nrow(result), 2)
  expect_true(inherits(result, c("tbl_df", "tbl", "data.frame")))
})

# vcr::use_cassette("IA_GBIF_show_values", {
test_that("show_values works for GBIF fields", {
  search_fields("basisOfRecord") |>
    show_values() |>
    nrow() |>
    expect_gt(1)
})

# vcr::use_cassette("IA_GBIF_atlas_counts", {
test_that("atlas_counts works for GBIF", {
  expect_gt(atlas_counts()$count, 0)
})

test_that("atlas_counts fails for GBIF when type = 'species'", {
  expect_error(atlas_counts(type = "species"))
})

# vcr::use_cassette("IA_GBIF_atlas_counts_identify", {
test_that("atlas_counts works with galah_identify for GBIF", {
  result <- galah_call() |>
    galah_identify("Mammalia") |>
    atlas_counts()
  expect_gt(result$count, 1)
})

# vcr::use_cassette("IA_GBIF_atlas_counts_group_by", {
test_that("atlas_counts works with group_by for GBIF", {
  result <- galah_call() |>
    filter(year >= 2020) |>
    group_by(year) |>
    count() |>
    collect()
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("`galah_select()` returns message for GBIF", {
  expect_message({x <- galah_select(galah_call())})
  expect_true(is.null(x$select))
  expect_message({x <- select(galah_call())})
  expect_true(is.null(x$select))
})

# test_that("atlas_species works for GBIF", {
#   skip_on_cran()
#   species <- galah_call() |>
#     galah_identify("perameles") |>
#     atlas_species()
#   expect_gt(nrow(species), 0)
#   expect_gt(ncol(species), 0)
#   expect_true(inherits(species, c("tbl_df", "tbl", "data.frame")))
# })

test_that("atlas_media fails for GBIF", {
  skip_on_cran()
  expect_error({galah_call() |>
    galah_identify("perameles") |>
    atlas_media()
  })
})

test_that("`collapse()` et al. work for GBIF", {
  skip_on_cran()
  
  # collapse
  gbif_query <- galah_call() |>
    identify("Vulpes vulpes") |>
    filter(year <= 1800, 
           basisOfRecord == "PRESERVED_SPECIMEN") |>
    collapse(type = "occurrences")
  
  # compute
  gbif_response <- compute(gbif_query)
  expect_true(inherits(gbif_response, "data_response"))
  expect_equal(length(gbif_response), 1)
  
  # collect
  occ <- collect(gbif_response)
  expect_gt(nrow(occ), 0)
  expect_gt(ncol(occ), 0)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

galah_config(atlas = "Australia")