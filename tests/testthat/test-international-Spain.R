# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Spain works", {
  expect_message(galah_config(atlas = "Spain"))
})

test_that("show_all(fields) works for Spain", {
  skip_if_offline()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(licences) works for Spain", {
  skip_if_offline()
  x <- show_all(licences) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Spain", {
  skip_if_offline()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Spain", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Spain", {
  skip_if_offline()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Spain", {
  skip_if_offline()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Spain", {
  skip_if_offline()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) works for Spain", {
  skip_if_offline()
  x <- show_all(profiles) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(lists) works for Spain", {
  skip_if_offline()
  x <- show_all(lists, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for Spain", {
  skip_if_offline()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Spain", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works using data.frames for Spain", {
  skip_if_offline()
  x <- search_all(taxa, 
                  data.frame(kingdom = "Animalia", 
                             phylum = "Chordata")) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(identifiers) works for Spain", {
  skip_if_offline()
  x <- search_all(identifiers, "359") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Spain", {
  skip_if_offline()
  x <- search_all(fields, "basisOfRecord") |> 
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for profiles for Spain", {
  skip_if_offline()
  x <- search_all(profiles, "LA") |> 
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works for Spain", {
  skip_if_offline()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Spain", {
  skip_if_offline()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with galah_identify for Spain", {
  skip_if_offline()
  result <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    filter(class == "Mammalia") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result2, "try-error"), message = "API not available")
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})

test_that("atlas_counts works with group_by for Spain", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2000) |>
    group_by(basis_of_record) |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("basis_of_record", "count"))
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
  skip_if_offline()
  x <- galah_select()
  y <- galah_select(basisOfRecord)
  expect_equal(length(x), 2)
  expect_equal(x$summary, "group = basic")
  expect_equal(x$group, "basic")
  expect_true(inherits(x, c("list"))) 
  expect_equal(length(y), 3)
  expect_equal(y$summary, "basisOfRecord")
  expect_equal(y$group, character(0))
  expect_true(inherits(y, c("list"))) 
  expect_true(inherits(y[[1]], c("quosure", "formula"))) 
})

# Occurrences working as of 2023/10/31
test_that("atlas_occurrences works for Spain", {
  skip_on_cran()
  skip_if_offline()
  galah_config(
    atlas = "Spain",
    email = "test@ala.org.au",
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year <= 1800) |>
    galah_select(species, year) |>
    atlas_occurrences() |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

galah_config(atlas = "Australia")
