# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Guatemala works", {
  expect_message(galah_config(atlas = "Guatemala"))
})

test_that("show_all(fields) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(collections) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
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
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "basis_of_record") |> 
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Guatemala", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

## FIXME: Both queries work, but results are notably different
## update 2023-11-03: still happens, but unclear if an API problem or a data problem
# test_that("atlas_counts works with galah_identify for Guatemala", {
#   skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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

test_that("atlas_counts works with group_by for Guatemala when count = 0", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("Mammalia") |>
    filter(!is.na(all_image_url)) |>
    group_by("family") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

test_that("atlas_species fails for Guatemala due to unavailable API", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Guatemala",
    email = "test@ala.org.au", 
    send_email = FALSE)
  expect_error({galah_call() |>
    galah_identify("Carnivora") |>
    atlas_species()})
})

test_that("atlas_occurrences works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Guatemala",
    email = "test@ala.org.au", 
    send_email = FALSE)
  occ_collapse <- galah_call() |>
    identify("Mammalia") |>
    filter(year <= 1900) |>
    # select(taxon_name, year)
    collapse() |>
    try(silent = TRUE)
  skip_if(inherits(occ_collapse, "try-error"), message = "API not available")
  expect_s3_class(occ_collapse, "query")
  expect_equal(names(occ_collapse), 
               c("type", "url", "headers", "filter"))
  occ_compute <- compute(occ_collapse) |>
    try(silent = TRUE)
  skip_if(inherits(occ_compute, "try-error"), message = "API not available")
  expect_s3_class(occ_compute, "computed_query")
  occ <- occ_compute |>
    collect(wait = TRUE) |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 8)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_media() works for Guatemala", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Guatemala",
    email = "test@ala.org.au",
    # download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    identify("Mammalia") |>
    filter(year == 2020) |>
    #        !is.na(all_image_url)) |>
    # count() |>
    # collect()
    atlas_media() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(x), 1)
  expect_equal(colnames(x)[1:2],
               c("media_id", "recordID"))
  # download a subset
  n_downloads <- 5
  collect_media(x[seq_len(n_downloads), ])
  expect_equal(length(list.files("temp", pattern = ".jpg$")),
               n_downloads)
  unlink("temp", recursive = TRUE)
})

galah_config(atlas = "Australia")
