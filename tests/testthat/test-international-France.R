context("Test international atlases: France")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = France works", {
  expect_message(galah_config(atlas = "France"))
})

test_that("show_all(fields) works for France", {
  vcr::use_cassette("IA_France_show_all_fields", {
    x <- show_all_fields()
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for France", {
  vcr::use_cassette("IA_France_show_all_collections", {
    x <- show_all(collections)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for France", {
  vcr::use_cassette("IA_France_show_all_datasets", {
    x <- show_all(datasets)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for France", {
  vcr::use_cassette("IA_France_show_all_providers", {
    x <- show_all(providers)
  })
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for France", {
  expect_error(show_all(profiles))
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
  expect_error(show_all(profiles))
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




vcr::use_cassette("IA_France_show_values", {
  test_that("show_values works for France", {
    
    search_fields("basis_of_record") |> 
      show_values() |>
      nrow() |>
      expect_gt(1)
    
    search_lists("a_list") |> 
      show_values() |>
      expect_error()
    
    search_profiles("profile") |> 
      show_values() |>
      expect_error()
  })
})

vcr::use_cassette("IA_France_atlas_counts", {
  test_that("atlas_counts works for France", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

# vcr::use_cassette("IA_France_atlas_counts2", {
#   test_that("atlas_counts works with galah_identify for France", { # FIXME: galah_identify does returns count of 0
#     skip_on_cran()
#     result <- galah_call() |>
#       galah_identify("Mammalia") |>
#       atlas_counts()
# 
#     result2 <- galah_call() |>
#       galah_filter(class == "Mammalia") |>
#       atlas_counts()
# 
#     expect_lt(
#       sqrt((result2$count - result$count)^2) / result$count,
#       0.1) # i.e. <1% margin of error
#   })
# })

vcr::use_cassette("IA_France_atlas_counts3", {
  test_that("atlas_counts works with group_by for France", {
    skip_on_cran()
    result <- galah_call() |>
      galah_filter(year >= 2018) |>
      galah_group_by(basis_of_record) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("basis_of_record", "count"))
  })
})

test_that("atlas_occurrences returns error for France", {
  expect_error(atlas_occurrences(
    filter = galah_filter(year == 2020)
  ))
})

galah_config(atlas = "Australia")