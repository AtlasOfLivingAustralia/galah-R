# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Guatemala works", {
  expect_message(galah_config(atlas = "Guatemala"))
})

test_that("show_all(fields) works for Guatemala", {
  skip_if_offline()
  x <- show_all(fields)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(collections) works for Guatemala", {
  skip_if_offline()
  x <- show_all(collections, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Guatemala", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Guatemala", {
  skip_if_offline()
  x <- show_all(providers, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Guatemala", {
  skip_if_offline()
  x <- show_all(reasons)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Guatemala", {
  skip_if_offline()
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Guatemala", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for Guatemala", {
  expect_error(show_all(profiles))
})

test_that("search_all(fields) works for Guatemala", {
  skip_if_offline()
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Guatemala", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Guatemala", {
  skip_if_offline()
  x <- search_all(fields, "basis_of_record") |> 
    show_values()
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works for Guatemala", {
  skip_if_offline()
  expect_gt(atlas_counts()$count, 0)
  expect_gt(atlas_counts(type = "species")$count, 0)
})

## FIXME: Both queries work, but results are notably different
## update 2023-11-03: still happens, but unclear if an API problem or a data problem
# test_that("atlas_counts works with galah_identify for Guatemala", {
#   skip_if_offline()
#   result <- galah_call() |>
#     identify("Mammalia") |>
#     count() |>
#     collect()
#   expect_gt(result$count, 1)
#   result2 <- galah_call() |>
#     filter(class == "Mammalia") |>
#     count() |>
#     collect()
#   pc_difference <- sqrt((result2$count - result$count)^2) / result$count
#   expect_lt(pc_difference, 0.1) # i.e. <1% margin of error
# })

test_that("atlas_counts works with group_by for Guatemala", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2000) |>
    group_by(basis_of_record) |>
    count() |>
    collect()
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("basis_of_record", "count"))
})

test_that("atlas_occurrences works for Guatemala", {
  skip_if_offline()
  galah_config(
    atlas = "Guatemala",
    email = "test@ala.org.au", 
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year <= 1900) |>
    galah_select(taxon_name, year) |>
    atlas_occurrences()   
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
})

galah_config(atlas = "Australia")