# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Kew works", {
  expect_message(galah_config(atlas = "Kew",
                              email = "ala4r@ala.org.au",
                              password = "galah-Kew-test-login"))
})

test_that("show_all(fields) works for Kew", {
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

test_that("show_all(licences) works for Kew", {
  # NOTE: When tested on 2025-07-04 this API works, but contains no data
  skip_if_offline(); skip_on_ci()
  x <- show_all(licences) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 0)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) unavailable for Kew", {
  show_all(profiles) |>
    expect_error(label = "No API is available for type `metadata/profiles`")
})

##  NOTE: When tested on 2025-07-04 this API works, but contains no data
# test_that("show_all(lists) works for Kew", {
#  skip_if_offline(); skip_on_ci()
#  x <- show_all(lists, limit = 10) |>
#    try(silent = TRUE)
#  skip_if(inherits(x, "try-error"), message = "API not available")
#  expect_lte(nrow(x), 10)
#  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
# })

test_that("search_all(fields) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, "Acer") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(identifiers) unavailable for Kew", {
  x <- search_all(identifiers, "359") |>
    expect_error(label = "No API is available for type `metadata/identifiers`")
})

## Currently failing; API exists but doesn't return data
## Server-side problem?
# test_that("show_values works for fields for Kew", {
#   skip_if_offline(); skip_on_ci()
#   x <- search_all(fields, "basis_of_record") |> 
#     show_values() |>
#     try(silent = TRUE)
#   skip_if(inherits(x, "try-error"), message = "API not available")
#   expect_gte(nrow(x), 1)
#   expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
# })

test_that("atlas_counts works for Kew", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts() |>
    dplyr::pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

## Currently failing: see `show_values()` comment above
## which calls the same API
# test_that("atlas_counts works with type = 'species' for Kew", {
#   skip_if_offline(); skip_on_ci()
#   x <- atlas_counts(type = "species") |>
#     pull(count) |>
#     try(silent = TRUE)
#   skip_if(inherits(x, "try-error"), message = "API not available")
#   expect_gt(x, 0)
# })

test_that("`atlas_counts()` works with `identify()` for Kew", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("Acer") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    filter(genus == "Acer") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result2, "try-error"), message = "API not available")
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})

## Again, facetting not functional yet
# test_that("`atlas_counts()` works with `group_by()` for Kew", {
#   skip_if_offline(); skip_on_ci()
#   result <- galah_call() |>
#     filter(year >= 2000) |>
#     group_by(basis_of_record) |>
#     count() |>
#     collect() |>
#     try(silent = TRUE)
#   skip_if(inherits(result, "try-error"), message = "API not available")
#   expect_gt(nrow(result), 1)
#   expect_equal(names(result), c("basis_of_record", "count"))
#   })

test_that("`atlas_species()` works for Kew", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Kew",
    email = "ala4r@ala.org.au",
    send_email = FALSE)
  spp <- galah_call() |>
    identify("Pinus") |>
    atlas_species() |>
    try(silent = TRUE)
  skip_if(inherits(spp, "try-error"), message = "API not available")
  expect_gt(nrow(spp), 20)
  expect_gte(ncol(spp), 10)
  expect_s3_class(spp, c("tbl_df", "tbl", "data.frame"))
})

test_that("`atlas_occurrences()` works for Kew", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Kew",
    email = "ala4r@ala.org.au",
    download_reason_id = 10,
    send_email = FALSE)
  occ <- galah_call() |>
    identify("Acer") |>
    filter(year >= 1999) |>
    select(species, year) |>
    atlas_occurrences() |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_false(any(occ$year < 1999))
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

test_that("`atlas_media()` works for Kew", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Kew",
    email = "ala4r@ala.org.au",
    download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    identify("Acer") |>
    filter(year >= 2020) |>
    atlas_media() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(x), 1)
  expect_equal(colnames(x)[1:2],
               c("media_id", "media_type"))
  # download a subset
  quiet_media <- function(...){
    x <- purrr::quietly(collect_media)
    x(...)$result
  }
  n_downloads <- min(c(nrow(x), 5))
  quiet_media(x[seq_len(n_downloads), ])
  expect_equal(length(list.files("temp", pattern = ".jpg$")),
               n_downloads)
  unlink("temp", recursive = TRUE)
})

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config)