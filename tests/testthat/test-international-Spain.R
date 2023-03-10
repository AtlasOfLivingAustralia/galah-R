context("Test international atlases: Spain")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Spain works", {
  expect_message(galah_config(atlas = "Spain"))
})

test_that("show_all(fields) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_fields", {
    x <- show_all(fields)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(licences) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_licences", {
    x <- show_all(licences)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_collections", {
    x <- show_all(collections, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_datasets", {
    x <- show_all(datasets, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_providers", {
    x <- show_all(providers, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_reasons", {
    x <- show_all(reasons)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_assertions", {
    x <- show_all(assertions)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_profiles", {
    x <- show_all(profiles)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(lists) works for Spain", {
  vcr::use_cassette("IA_Spain_show_all_lists", {
    x <- show_all(lists, limit = 10)
  })
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for Spain", {
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Spain", {
  vcr::use_cassette("IA_Spain_search_all_taxa", {
    x <- search_all(taxa, "Mammalia")
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works using data.frames for Spain", {
  vcr::use_cassette("IA_Spain_search_all_taxa_df", {
    x <- search_all(taxa, 
                    data.frame(kingdom = "Animalia", 
                               phylum = "Chordata"))
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(identifiers) works for Spain", {
  vcr::use_cassette("IA_Spain_search_all_identifiers", {
    x <- search_all(identifiers, "359")
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Spain", {
  vcr::use_cassette("IA_Spain_show_values_fields", {
    x <- search_all(fields, "basis_of_record") |> 
      show_values()
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for profiles for Spain", {
  vcr::use_cassette("IA_Spain_show_values_profiles", {
   x <- search_all(profiles, "LA") |> 
      show_values()
  })
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

vcr::use_cassette("IA_Spain_atlas_counts", {
  test_that("atlas_counts works for Spain", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_Spain_atlas_counts_identify", {
  test_that("atlas_counts works with galah_identify for Spain", {
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

vcr::use_cassette("IA_Spain_atlas_counts_group_by", {
  test_that("atlas_counts works with group_by for Spain", {
    result <- galah_call() |>
      galah_filter(year >= 2000) |>
      galah_group_by(basis_of_record) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("basis_of_record", "count"))
  })
})

## profiles system is available for Spain, but not implemented in biocache
# test_that("galah_apply_profile filters counts for Spain", {
#   vcr::use_cassette("IA_Spain_apply_profile_counts", {
#     without_profile <- galah_call() |>
#       atlas_counts()
#     with_profile <- galah_call() |>
#       galah_apply_profile(LA) |>
#       atlas_counts()
#   })
#   
#   expect_gt(with_profile[[1]], 0)
#   expect_equal(class(without_profile), class(with_profile))
#   expect_lt(with_profile[[1]], without_profile[[1]])
# })

test_that("galah_select works for Spain", {
  x <- galah_select()
  expect_gt(nrow(x), 0)
  expect_equal(ncol(x), 2)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame"))) 
})

# test_that("atlas_occurrences works for Spain", {
#   skip_on_cran()
#   galah_config(
#     atlas = "Spain",
#     email = "test@ala.org.au",
#     send_email = FALSE)
#   occ <- galah_call() |>
#     galah_identify("Mammalia") |>
#     galah_filter(year <= 1800) |>
#     galah_select(species, year) |>
#     atlas_occurrences()
#   expect_gt(nrow(occ), 0)
#   expect_equal(ncol(occ), 2)
#   expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
# })

galah_config(atlas = "Australia")
