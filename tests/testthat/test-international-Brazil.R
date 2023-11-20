# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Brazil works", {
  expect_message(galah_config(atlas = "Brazil"))
})

test_that("show_all(fields) works for Brazil", {
  skip_if_offline()
  x <- show_all(fields)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Brazil", {
  skip_if_offline()
  x <- show_all(collections, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Brazil", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Brazil", {
  skip_if_offline()
  x <- show_all(providers, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for Brazil", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for Brazil", {
  skip_if_offline()
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Brazil", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for Brazil", {
  skip_if_offline()
  x <- show_all(lists, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for Brazil", {
  skip_if_offline()
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Brazil", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for Brazil", {
  skip_if_offline()
  search_fields("basis_of_record") |>
    show_values() |>
    nrow() |>
    expect_gt(1)
  search_lists("drt1565630923841") |>
    show_values() |>
    nrow() |>
    expect_gt(1)
  search_profiles("profile") |>
    show_values() |>
    expect_error()
})

test_that("atlas_counts works for Brazil", {
  skip_if_offline()
  expect_gt(atlas_counts()$count, 0)
  expect_gt(atlas_counts(type = "species")$count, 0)
})

test_that("atlas_counts works with galah_identify for Brazil", {
  skip_if_offline()
  result <- galah_call() |>
    galah_identify("Mammalia") |> # Works when run_checks = TRUE
    atlas_counts()
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    galah_filter(class == "Mammalia") |>
    atlas_counts()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count,
    0.1) # i.e. <1% margin of error
})

test_that("atlas_counts works with group_by for Brazil", {
  skip_if_offline()
  result <- galah_call() |>
    galah_filter(year >= 2020) |>
    galah_group_by(year) |>
    atlas_counts()
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

## FIXME: Caused by taxonomic search issue
test_that("atlas_occurrences works for Brazil", {
  skip_if_offline()
  skip_on_cran()
  galah_config(
    atlas = "Brazil",
    email = "ala4r@ala.org.au", 
    run_checks = FALSE,
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year == 1970) |>
    galah_select(taxon_name, year) |>
    atlas_occurrences()  
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

# example species from Brazil:
  # "Ramphastos toco" # toucan
  # "Myrmecophaga tridactyla" # anteater

galah_config(atlas = "Australia")