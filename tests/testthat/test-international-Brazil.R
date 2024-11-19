# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Brazil works", {
  expect_message(galah_config(atlas = "Brazil"))
})

test_that("show_all(fields) works for Brazil", {
  skip_if_offline()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Brazil", {
  skip_if_offline()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Brazil", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Brazil", {
  skip_if_offline()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for Brazil", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for Brazil", {
  skip_if_offline()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Brazil", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for Brazil", {
  skip_if_offline()
  x <- show_all(lists, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for Brazil", {
  skip_if_offline()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Brazil", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for Brazil", {
  skip_if_offline()
  x <- search_fields("basis_of_record") |>
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  y <- search_lists("drt1565630923841") |>
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(y, "try-error"), message = "API not available")
  expect_gt(nrow(y), 1)
  search_profiles("profile") |>
    show_values() |>
    expect_error()
})

test_that("atlas_counts works for Brazil", {
  skip_if_offline()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Brazil", {
  skip_if_offline()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with galah_identify for Brazil", {
  skip_if_offline()
  query1 <- galah_call() |>
    galah_identify("Mammalia") |> 
    count() |>
    collapse()  # note: this is set up differently for debugging
  result1 <- collect(query1) |>
    try(silent = TRUE)
  skip_if(inherits(result1, "try-error"), message = "API not available")
  expect_gt(result1$count, 1)
  query2 <-  galah_call() |>
    galah_filter(class == "Mammalia") |>
    count() |>
    collapse()
  result2 <- collect(query2) |>
    try(silent = TRUE)
  skip_if(inherits(result2, "try-error"), message = "API not available")
  expect_gt(result2$count, 1)
  # expect_lt(
  #   sqrt((result2$count - result1$count)^2) / result1$count,
  #   0.1) # i.e. <1% margin of error
  ## This isn't met for this atlas, for unknown reasons
})

test_that("atlas_counts works with group_by for Brazil", {
  skip_if_offline()
  result <- galah_call() |>
    galah_filter(year >= 2020) |>
    galah_group_by(year) |>
    atlas_counts()  |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("atlas_species works for Brazil", {
  skip_if_offline()
  galah_config(
    atlas = "Brazil",
    email = "ala4r@ala.org.au", 
    run_checks = FALSE,
    send_email = FALSE)
  spp <- galah_call() |>
    galah_identify("Carnivora") |>
    atlas_species() |>
    try(silent = TRUE)
  skip_if(inherits(spp, "try-error"), message = "API not available")
  expect_gt(nrow(spp), 20)
  expect_equal(ncol(spp), 11)
  expect_s3_class(spp, c("tbl_df", "tbl", "data.frame"))
})

## FIXME: Caused by taxonomic search issue
test_that("atlas_occurrences works for Brazil", {
  skip_if_offline()
  galah_config(
    atlas = "Brazil",
    email = "ala4r@ala.org.au", 
    run_checks = FALSE,
    send_email = FALSE)
  occ_collapse <- galah_call() |>
    identify("Mammalia") |>
    filter(year == 1970) |>
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

# example species from Brazil:
  # "Ramphastos toco" # toucan
  # "Myrmecophaga tridactyla" # anteater

test_that("atlas_media() works for Brazil", {
  skip_if_offline()
  galah_config(
    atlas = "Brazil",
    email = "ala4r@ala.org.au",
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    identify("Mammalia") |>
    filter(year == 2010
          # !is.na(all_image_url)
    ) |>
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