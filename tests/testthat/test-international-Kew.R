# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Kew works", {
  expect_message(galah_config(atlas = "Kew",
                              email = "ala4r@ala.org.au",
                              password = "galah-Kew-test-login"))
})

test_that("show_all(fields) works for Kew", {
  skip_galah()
  x <- show_all(fields) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_tibble(x)
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(licences) fails for Kew", {
  show_all(licences) |>
    expect_error()
})

test_that("show_all(collections) fails for Kew", {
  show_all(collections, limit = 10)  |>
    expect_error()
})

test_that("show_all(datasets) fails for Kew", {
  show_all(datasets, limit = 10)  |>
    expect_error()
})

test_that("show_all(providers) fails for Kew", {
  show_all(providers, limit = 10)  |>
    expect_error()
})

test_that("show_all(reasons) works for Kew", {
  skip_galah()
  x <- show_all(reasons) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_tibble(x)
})

test_that("show_all(assertions) works for Kew", {
  skip_galah()
  x <- show_all(assertions) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_tibble(x)
})

test_that("show_all(profiles) unavailable for Kew", {
  show_all(profiles) |>
    expect_error(label = "No API is available for type `metadata/profiles`")
})

##  NOTE: When tested on 2025-07-04 this API works, but contains no data
# test_that("show_all(lists) works for Kew", {
#  skip_galah()
#  x <- show_all(lists, limit = 10) |>
#    skip_unavailable()
#  expect_lte(nrow(x), 10)
#  expect_tibble(x)
# })

test_that("search_all(fields) works for Kew", {
  skip_galah()
  x <- search_all(fields, "year") |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_tibble(x)
})

test_that("search_all(taxa) works for Kew", {
  skip_galah()
  x <- search_all(taxa, "Acer") |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_tibble(x)
})

test_that("search_all(identifiers) unavailable for Kew", {
  x <- search_all(identifiers, "359") |>
    expect_error(label = "No API is available for type `metadata/identifiers`")
})

## Currently failing; API exists but doesn't return data
## Server-side problem?
# test_that("show_values works for fields for Kew", {
#   skip_galah()
#   x <- search_all(fields, "basis_of_record") |> 
#     show_values() |>
#     skip_unavailable()
#   expect_gte(nrow(x), 1)
#   expect_tibble(x)
# })

test_that("atlas_counts works for Kew", {
  skip_galah()
  x <- atlas_counts() |>
    skip_unavailable()
  expect_tibble(x)
  x |>
    dplyr::pull(count) |>
    expect_gt(0)
})

## Currently failing: see `show_values()` comment above
## which calls the same API
# test_that("atlas_counts works with type = 'species' for Kew", {
#   skip_galah()
#   x <- atlas_counts(type = "species") |>
#     pull(count) |>
#     skip_unavailable()
#   expect_gt(x, 0)
# })

test_that("`atlas_counts()` works with `identify()` for Kew", {
  skip_galah()
  result <- galah_call() |>
    identify("Quercus robur") |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    filter(species == "Quercus robur") |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})

## Again, facetting not functional yet
# test_that("`atlas_counts()` works with `group_by()` for Kew", {
#   skip_galah()
#   result <- galah_call() |>
#     filter(year >= 2000) |>
#     group_by(basis_of_record) |>
#     count() |>
#     collect() |>
#     skip_unavailable()
#   expect_gt(nrow(result), 1)
#   expect_equal(names(result), c("basis_of_record", "count"))
#   })

# NOTE: Identify fails because the API returns species *within* pinus, 
# rather than the genus itself. The api accepts a `rank` argument
# to adjust for this, but it's inconsistent with other atlases, 
# so `galah` doesn't have syntax for that yet. Would be possible
# to have something like:
#
# request_metadata() |>
#   identify("Pinus") 
#   filter(rank == "genus")
#   collect()
test_that("`atlas_species()` works for Kew", {
  skip_galah()
  galah_config(
    atlas = "Kew",
    email = "ala4r@ala.org.au",
    send_email = FALSE)
  spp <- galah_call() |>
    filter(genus == "Pinus") |>
    atlas_species() |>
    skip_unavailable()
  expect_tibble(spp)
  expect_gt(nrow(spp), 20)
  expect_gte(ncol(spp), 10)
})

test_that("`atlas_occurrences()` works for Kew", {
  skip_galah()
  galah_config(
    atlas = "Kew",
    email = "ala4r@ala.org.au",
    download_reason_id = 10,
    send_email = FALSE)
  occ <- galah_call() |>
    filter(genus == "Pinus",
           year >= 1999) |>
    select(species, year) |>
    atlas_occurrences() |>
    skip_unavailable()
  expect_tibble(occ)
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_false(any(occ$year < 1999))
})

test_that("`atlas_media()` works for Kew", {
  skip_galah()
  galah_config(
    atlas = "Kew",
    email = "ala4r@ala.org.au",
    download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    identify("Quercus robur") |>
    filter(year >= 2020) |>
    atlas_media() |>
    skip_unavailable()
  expect_tibble(x)
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
