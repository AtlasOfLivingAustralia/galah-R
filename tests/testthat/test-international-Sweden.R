# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Sweden works", {
  expect_message(galah_config(atlas = "Sweden"))
})

test_that("show_all(fields) works for Sweden", {
  skip_if_offline()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Sweden", {
  skip_if_offline()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Sweden", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Sweden", {
  skip_if_offline()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for Sweden", {
  skip_if_offline()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 0) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Sweden", {
  skip_if_offline()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for Sweden", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for Sweden", {
  expect_error(show_all(lists))
})

test_that("search_all(fields) works for Sweden", {
  skip_if_offline()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Sweden", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`search_taxa()` works for Sweden", {
  skip_if_offline()
  taxa <- search_taxa("Vulpes vulpes") |>
    try(silent = TRUE)
  skip_if(inherits(taxa, "try-error"), message = "API not available")
  expect_equal(nrow(taxa), 1)
  expect_gte(ncol(taxa), 3)
  expect_true(all(
    c("search_term", "scientific_name", "taxon_concept_id", "match_type") %in%
    colnames(taxa)))
})  

test_that("`search_taxa()` works for multiple queries in Sweden", {
  skip_if_offline()
  search_terms <- c("Rodentia", "Amphibia", "Serpentes")
  taxa <- search_taxa(search_terms) |>
    try(silent = TRUE)
  skip_if(inherits(taxa, "try-error"), message = "API not available")
  expect_equal(nrow(taxa), 3)
  expect_equal(taxa$search_term, search_terms)
  expect_true(all(taxa$match_type == "exactMatch"))
})

test_that("`search_taxa()` works for multiple ranks in Sweden", {
  skip_if_offline()
  taxa <-  data.frame(genus = c("Asteraceae", "Pinus"), kingdom = "Plantae") |>
    search_taxa() |>
    try(silent = TRUE)
  skip_if(inherits(taxa, "try-error"), message = "API not available")
  expect_equal(nrow(taxa), 2)
  expect_true(all(grepl("^[[:digit:]]+$", taxa$taxon_concept_id)))
})

test_that("show_values works for Sweden", {
  skip_if_offline()
  x <- search_fields("basis_of_record") |>
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
})

test_that("atlas_counts works with type = 'occurrences' for Sweden", {
  skip_if_offline()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Sweden", {
  skip_if_offline()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with galah_identify for Sweden", {
  skip_if_offline()
  result <- galah_call() |>
    galah_identify("Mammalia") |>
    atlas_counts() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    galah_filter(class == "Mammalia") |>
    atlas_counts() |>
    try(silent = TRUE)
  skip_if(inherits(result2, "try-error"), message = "API not available")
  expect_gt(result2$count, 1)
  error_rate <- sqrt((result2$count - result$count)^2) / result$count
  expect_true(error_rate < 0.1) # we expect a <1% margin of error
})

test_that("atlas_counts works with group_by for Sweden", {
  skip_if_offline()
  result <- galah_call() |>
    galah_filter(year >= 2020) |>
    galah_group_by(year) |>
    atlas_counts() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("atlas_occurrences works for Sweden", {
  skip_if_offline()
  galah_config(
    atlas = "Sweden",
    email = "martinjwestgate@gmail.com",
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year < 1850) |>
    galah_select(taxon_name, year) |>
    atlas_occurrences() |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
})

galah_config(atlas = "Australia")