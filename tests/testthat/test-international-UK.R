context("Test international atlases: United Kingdom")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = United Kingdom works", {
  expect_message(galah_config(atlas = "United Kingdom"))
})

test_that("show_all(fields) works for UK", {
  vcr::use_cassette("IA_UK_show_all_fields", {
    x <- show_all_fields()
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for UK", {
  vcr::use_cassette("IA_UK_show_all_collections", {
    x <- show_all(collections, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for UK", {
  vcr::use_cassette("IA_UK_show_all_datasets", {
    x <- show_all(datasets, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for UK", {
  vcr::use_cassette("IA_UK_show_all_providers", {
    x <- show_all(providers, limit = 10)
  })
  expect_lte(nrow(x), 10) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for UK", {
  vcr::use_cassette("IA_UK_show_all_reasons", {
    x <- show_all(reasons)
  })
  expect_gte(nrow(x), 0) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for UK", {
  vcr::use_cassette("IA_UK_show_all_assertions", {
    x <- show_all(assertions)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for UK", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for UK", {
  vcr::use_cassette("IA_UK_show_all_lists", {
    x <- show_all(lists)
  })
  expect_gte(nrow(x), 0) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for UK", {
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for UK", {
  vcr::use_cassette("IA_UK_search_all_taxa", {
    x <- search_all(taxa, "Mammalia")
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

vcr::use_cassette("IA_UK_search_taxa_multiple", {
  test_that("search_taxa works for multiple queries", {
    taxa <- search_taxa(c("Vulpes vulpes", "Meles meles"))
    expect_equal(nrow(taxa), 2)
  })
})

vcr::use_cassette("IA_UK_search_taxa_types", {
  test_that("search_taxa doesn't break with typos", {
    expect_silent(search_taxa("Vlpes"))
  })
})

test_that("show_values works for UK", {
  x <- search_fields("basis_of_record") 
  vcr::use_cassette("IA_UK_show_values_field", {
   y <- show_values(x)
  })
  expect_gt(nrow(y), 1)
})

test_that("show_list_values works for United Kingdom", {
  vcr::use_cassette("IA_UK_show_list_values", {
    x <- search_lists("dr556") |> 
      show_values()
  })
  expect_gt(nrow(x), 1)
})

vcr::use_cassette("IA_UK_atlas_counts_records", {
  test_that("atlas_counts w records works for United Kingdom", {
    expect_gt(atlas_counts()$count, 0)
  })
})

vcr::use_cassette("IA_UK_atlas_counts_species", {
  test_that("atlas_counts w species works for United Kingdom", {
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_UK_atlas_counts_identify", {
  test_that("atlas_counts works with galah_identify for United Kingdom", {
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

vcr::use_cassette("IA_UK_atlas_counts_group_by", {
  test_that("atlas_counts works with group_by for United Kingdom", {
    result <- galah_call() |>
      galah_filter(year >= 2020) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})

# FIXME: this is currently failing to download records
# test_that("atlas_occurrences works for United Kingdom", {
#   skip_on_cran()
#   galah_config(
#     atlas = "United Kingdom",
#     email = "ala4r@ala.org.au",
#     send_email = FALSE)
#   occ <- galah_call() |>
#     galah_identify("Mammalia") |>
#     galah_filter(year <= 1900 & basis_of_record == PreservedSpecimen) |>
#     galah_select(taxon_name, year) |>
#     atlas_occurrences()
#   expect_gt(nrow(occ), 0)
#   expect_equal(ncol(occ), 2)
#   expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
# })

galah_config(atlas = "Australia")
