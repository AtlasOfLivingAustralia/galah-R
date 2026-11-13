# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = United Kingdom works", {
  expect_message(galah_config(atlas = "United Kingdom"))
})

test_that("show_all(fields) works for UK", {
  skip_galah()
  x <- show_all(fields) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_tibble(x)
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(collections) works for UK", {
  skip_galah()
  x <- show_all(collections, limit = 10) |>
    skip_unavailable()
  expect_lte(nrow(x), 10)
  expect_tibble(x)
})

test_that("show_all(datasets) works for UK", {
  skip_galah()
  x <- show_all(datasets, limit = 10) |>
    skip_unavailable()
  expect_lte(nrow(x), 10)
  expect_tibble(x)
})

test_that("show_all(providers) works for UK", {
  skip_galah()
  x <- show_all(providers, limit = 10) |>
    skip_unavailable()
  expect_lte(nrow(x), 10) # no data at present
  expect_tibble(x)
})

test_that("show_all(reasons) works for UK", {
  skip_galah()
  x <- show_all(reasons) |>
    skip_unavailable()
  expect_gte(nrow(x), 0) # no data at present
  expect_tibble(x)
})

test_that("show_all(assertions) works for UK", {
  skip_galah()
  x <- show_all(assertions) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_tibble(x)
})

test_that("show_all(profiles) fails for UK", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for UK", {
  skip_galah()
  x <- show_all(lists) |>
    skip_unavailable()
  expect_gte(nrow(x), 0)
  expect_tibble(x)
})

test_that("search_all(fields) works for UK", {
  skip_galah()
  x <- search_all(fields, "year") |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_tibble(x)
})

test_that("search_all(taxa) works for UK", {
  skip_galah()
  x <- search_all(taxa, "Mammalia") |>
    skip_unavailable()
  expect_gte(nrow(x), 1)
  expect_tibble(x)
})

test_that("search_taxa works for multiple queries", {
  skip_galah()
  taxa <- search_taxa(c("Vulpes vulpes", "Meles meles")) |>
    skip_unavailable()
  expect_equal(nrow(taxa), 2)
})

test_that("search_taxa doesn't break with typos", {
  skip_galah()
  expect_silent(search_taxa("Vlpes"))
})

test_that("show_values works for UK", {
  skip_galah()
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  
  # fields
  x <- search_fields("basisOfRecord") |>
    show_values() |>
    skip_unavailable()
  expect_gt(nrow(x), 1)

  # lists
  x <- search_lists("dr1445") |> 
    show_values() |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
})

test_that("`atlas_counts()` works with type = 'occurrences' for United Kingdom", {
  skip_galah()
  x <- atlas_counts() |>
    skip_unavailable()
  expect_tibble(x)
  x |>
    dplyr::pull(count) |>
    expect_gt(0)
})

test_that("`atlas_counts()` works with type = 'species' for United Kingdom", {
  skip_galah()
  x <- atlas_counts(type = "species") |>
    skip_unavailable()
  expect_tibble(x)
  x |>
    dplyr::pull(count) |>
    expect_gt(0)
})

test_that("`count()` works with `identify()` for United Kingdom", {
  skip_galah()
  result <- galah_call() |>
    identify("Vulpes") |>
    count() |>
    collect() |>
skip_unavailable()
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    filter(genus == "Vulpes") |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})
# Note: canonical example is to use class == Mammalia, but that fails,
# possibly because name-matching is going wrong somewhere
  
test_that("`count()` works with group_by for United Kingdom", {
  skip_galah()
  result <- galah_call() |>
    filter(year >= 2020) |>
    group_by(year) |>
    count() |>
    collect() |>
skip_unavailable()
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("`atlas_species()` works for United Kingdom", {
  skip_galah()
  galah_config(
    atlas = "United Kingdom",
    email = "ala4r@ala.org.au",
    run_checks = TRUE,
    download_reason_id = 10,
    send_email = FALSE)
  x <- galah_call() |>
      identify("Canidae") |>
      atlas_species()
  expect_tibble(x)
  expect_gte(nrow(x), 1)
  expect_gte(ncol(x), 1)
})

test_that("`atlas_occurrences()` works for United Kingdom", {
  skip_galah()
  galah_config(
    atlas = "United Kingdom",
    email = "ala4r@ala.org.au",
    run_checks = TRUE,
    download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  base_query <- galah_call() |>
    identify("Vulpes") |>
    filter(year <= 1800)
  counts <- base_query |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_gte(counts$count[1], 0) # i.e. non-zero count
  occ_collapse <- base_query |>
    select(group = "basic") |>
    collapse() |>
    skip_unavailable()
  # with run checks, this gives n = 5. Without it's n = 2
  expect_s3_class(occ_collapse, "query")
  expect_equal(names(occ_collapse), 
               c("type", "atlas", "url", "headers", "request"))
  expect_equal(occ_collapse$type, "data/occurrences")
  # compute
  # notes: 
    # no 'true' compute stage for NBN; collect() sends the query and retrieves data - unlike the other living atlases. 
    # sourceTypeId not required, but if specified, should be 2001 (ALA4R) not 2004 (galah)
    # no email needed for NBN
  occ_compute <- compute(occ_collapse)
  expect_s3_class(occ_compute, "computed_query")
  # collect
  Sys.sleep(20)
  occ <- collect(occ_compute, wait = FALSE) |>
    skip_unavailable(other_classes = "computed_query")
  expect_tibble(occ)
  expect_equal(nrow(occ), counts$count[1])
  expect_equal(ncol(occ), length(default_columns()))
  # expect_equal(colnames(occ), default_columns()) # users must request NBN-specific fields;
    # but Darwin-Core is returned.
  unlink("temp", recursive = TRUE)
})

test_that("`atlas_media()` works for UK", {
  skip_galah()
  galah_config(
    atlas = "United Kingdom",
    email = "ala4r@ala.org.au",
    run_checks = TRUE,
    download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    filter(year >= 2020 & year <= 2023) |>
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
  n_downloads <- 5
  quiet_media(x[seq_len(n_downloads), ])
  expect_equal(length(list.files("temp", pattern = ".jpg$")),
               n_downloads)
  unlink("temp", recursive = TRUE)
})

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config)
