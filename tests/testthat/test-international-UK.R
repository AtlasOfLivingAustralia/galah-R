# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = United Kingdom works", {
  expect_message(galah_config(atlas = "United Kingdom"))
})

test_that("show_all(fields) works for UK", {
  skip_if_offline()
  x <- show_all(fields)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(collections) works for UK", {
  skip_if_offline()
  x <- show_all(collections, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for UK", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for UK", {
  skip_if_offline()
  x <- show_all(providers, limit = 10)
  expect_lte(nrow(x), 10) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for UK", {
  skip_if_offline()
  x <- show_all(reasons)
  expect_gte(nrow(x), 0) # no data at present
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for UK", {
  skip_if_offline()
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for UK", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for UK", {
  skip_if_offline()
  x <- show_all(lists)
  expect_gte(nrow(x), 0)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for UK", {
  skip_if_offline()
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for UK", {
  skip_if_offline()
  x <- search_all(taxa, "Mammalia")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_taxa works for multiple queries", {
  skip_if_offline()
  taxa <- search_taxa(c("Vulpes vulpes", "Meles meles"))
  expect_equal(nrow(taxa), 2)
})

test_that("search_taxa doesn't break with typos", {
  skip_if_offline()
  expect_silent(search_taxa("Vlpes"))
})

test_that("show_values works for UK", {
  skip_if_offline()
  x <- search_fields("basis_of_record")
  y <- show_values(x)
  expect_gt(nrow(y), 1)
})

test_that("show_list_values works for United Kingdom", {
  skip_if_offline()
  x <- search_lists("dr556") |> 
    show_values()
  expect_gt(nrow(x), 1)
})

test_that("atlas_counts w records works for United Kingdom", {
  skip_if_offline()
  expect_gt(atlas_counts()$count, 0)
})

test_that("atlas_counts w species works for United Kingdom", {
  skip_if_offline()
  expect_gt(atlas_counts(type = "species")$count, 0)
})

test_that("atlas_counts works with galah_identify for United Kingdom", {
  skip_if_offline()
  result <- galah_call() |>
    identify("Vulpes") |>
    count() |>
    collect()
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    filter(genus == "Vulpes") |>
    count() |>
    collect()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})
# Note: canonical example is to use class == Mammalia, but that fails,
# possibly because name-matching is going wrong somewhere
  
test_that("atlas_counts works with group_by for United Kingdom", {
  skip_if_offline()
  result <- galah_call() |>
    galah_filter(year >= 2020) |>
    galah_group_by(year) |>
    atlas_counts()
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("atlas_occurrences works for United Kingdom", {
  skip_if_offline()
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
    collect() 
  expect_gte(counts$count[1], 0) # i.e. non-zero count
  occ_collapse <- base_query |>
    select(group = "basic") |>
    collapse()
  # with run checks, this gives n = 5. Without it's n = 2
  expect_s3_class(occ_collapse, "query_set")
  expect_equal(
    unlist(lapply(occ_collapse, function(a){a$type})),
    c("metadata/fields", 
      "metadata/assertions",
      "metadata/reasons",
      "metadata/taxa-single",
      "data/occurrences"))
  # compute
  # notes: 
    # no 'true' compute stage for NBN; collect() sends the query and retrieves data - unlike the other living atlases. 
    # sourceTypeId not required, but if specified, should be 2001 (ALA4R) not 2004 (galah)
    # no email needed for NBN
  occ_compute <- compute(occ_collapse)
  expect_s3_class(occ_compute, "query")
  expect_equal(names(occ_compute), 
               c("type", "url", "headers", "fields"))
  expect_equal(occ_compute$type, "data/occurrences")
  # collect
  occ <- collect(occ_compute)
  expect_equal(nrow(occ), counts$count[1])
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(occ), length(default_columns()))
  expect_equal(colnames(occ), default_columns())
  unlink("temp", recursive = TRUE)
})

galah_config(atlas = "Australia")