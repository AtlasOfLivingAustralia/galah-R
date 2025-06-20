# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = France works", {
  expect_message(galah_config(atlas = "France"))
})

test_that("show_all(fields) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10) # no data stored by this atlas at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) fails for France", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for France", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for France", {
  expect_error(show_all(lists))
})

test_that("search_all(fields) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for France", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, "Vulpes vulpes") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for France", {
  skip_if_offline(); skip_on_ci()
  x <- search_fields("basisOfRecord") |> 
      show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
})

test_that("atlas_counts works for France", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for France", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with galah_identify for France", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
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

test_that("atlas_counts works with group_by for France", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    filter(year >= 2018) |>
    group_by(year) |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("atlas_species works for France", {
  skip_if_offline(); skip_on_ci()
  galah_config(email = "ala4r@ala.org.au")
  x <- galah_call(type = "species") |>
    identify("Lagomorpha") |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_gt(nrow(x), 1)
  expect_equal(ncol(x), 11)
  expect_equal(colnames(x)[1], "taxon_concept_id")
})

test_that("atlas_occurrences works for France", {
  skip_if_offline(); skip_on_ci()
  galah_config(atlas = "France",
               email = "ala4r@ala.org.au")
  base_query <- galah_call() |>
    filter(year <= 1950,
           genus == "Vulpes")
  counts <- base_query |>
    count() |>
    collect()
  occ_collapse <-  base_query |>
    # select(species, year) |>
    collapse() |>
    try(silent = TRUE)
  skip_if(inherits(occ_collapse, "try-error"), message = "API not available")
  expect_s3_class(occ_collapse, "query")
  expect_equal(names(occ_collapse), 
               c("type", "url", "headers", "filter"))
  expect_equal(occ_collapse$type, "data/occurrences")
  # compute
  occ_compute <- compute(occ_collapse) |>
    try(silent = TRUE)
  skip_if(inherits(occ_compute, "try-error"), message = "API not available")
  expect_s3_class(occ_compute, "computed_query")
  # collect
  occ <- collect(occ_compute) |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_equal(nrow(occ), counts$count[1])
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(occ), 8)
  unlink("temp", recursive = TRUE)
})

galah_config(atlas = "Australia")
