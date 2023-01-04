context("Test international atlases: Brazil")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = GBIF works", {
  expect_message(galah_config(atlas = "GBIF",
                              username = "atlasoflivingaustralia",
                              email = "ala4r@ala.org.au",
                              password = "galah-gbif-test-login"))
})

vcr::use_cassette("IA_GBIF_show_all", {
  test_that("show_all works for GBIF", {
    ## collectory (note hard-coded limits for this API)
    expect_equal(nrow(show_all(collections)), 20)
    expect_equal(nrow(show_all(providers)), 20)
    expect_equal(nrow(show_all(datasets)), 20)
    ## records
    expect_gt(nrow(show_all(assertions)), 1) # works
    expect_gt(nrow(show_all(fields)), 1) # works
    # logger
    expect_error(show_all(reasons))
    # profiles
    expect_error(show_all(profiles))
    # lists
    expect_error(show_all(lists))
  })
}) 

vcr::use_cassette("IA_GBIF_search_all_taxa", {
  test_that("search_all(taxa) works for GBIF", {
    expect_equal(nrow(search_all(taxa, "Mammalia")), 1)
  })
})

vcr::use_cassette("IA_GBIF_search_all_datasets", {
  test_that("search_all(datasets) works for GBIF", {
    x <- expect_message(search_all(datasets, "Mammals"))
    expect_lte(nrow(x), 20)
    expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  })
})

vcr::use_cassette("IA_GBIF_search_all_collections", {
  test_that("search_all(collections) works for GBIF", {
    x <- expect_message(search_all(collections, "Museum"))
    expect_lte(nrow(x), 20)
    expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  })
})


vcr::use_cassette("IA_GBIF_search_all_providers", {
  test_that("search_all(providers) works for GBIF", {
    x <- expect_message(search_all(providers, "Frog"))
    expect_lte(nrow(x), 20)
    expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  })
})

test_that("search_all(fields) works for GBIF", {
  result <- search_all(fields, "year")
  expect_equal(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(result), 2)
})

vcr::use_cassette("IA_GBIF_show_values", {
  test_that("show_values works for GBIF fields", {
    search_fields("basisOfRecord") |>
      show_values() |>
      nrow() |>
      expect_gt(1)
  })
})

vcr::use_cassette("IA_GBIF_atlas_counts", {
  test_that("atlas_counts works for GBIF", {
    expect_gt(atlas_counts()$count, 0)
  })
})

test_that("atlas_counts fails for GBIF when type = 'species'", {
  expect_error(atlas_counts(type = "species"))
})

vcr::use_cassette("IA_GBIF_atlas_counts2", {
  test_that("atlas_counts works with galah_identify for GBIF", {
    result <- galah_call() |>
      galah_identify("Mammalia") |>
      atlas_counts()
    expect_gt(result$count, 1)
  })
})

vcr::use_cassette("IA_GBIF_atlas_counts3", {
  test_that("atlas_counts works with group_by for GBIF", {
    result <- galah_call() |>
      galah_filter(year >= 2020) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})

# up to here

test_that("atlas_species works for GBIF", {
  skip_on_cran()
})

test_that("atlas_occurrences works for GBIF", {
  skip_on_cran()
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year == 1970) |>
    galah_select(taxon_name, year) |>
    atlas_occurrences()  
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
})

galah_config(atlas = "Australia")