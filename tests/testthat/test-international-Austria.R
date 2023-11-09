# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Austria works", {
  expect_message(galah_config(atlas = "Austria"))
})

test_that("show_all(assertions) works for Austria", {
  skip_if_offline()
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "assertions") |> collect()
  expect_equal(x, y)
})

test_that("show_all(collections) works for Austria", {
  skip_if_offline()
  x <- show_all(collections)
  expect_gte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "collections") |> collect()
  expect_equal(x, y)
})

test_that("show_all(datasets) works for Austria", {
  skip_if_offline()
  x <- show_all(datasets)
  expect_gte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "datasets") |> collect()
  expect_equal(x, y)
})

test_that("show_all(fields) works for Austria", {
  skip_if_offline()
  x <- show_all(fields)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "fields") |> collect()
  expect_equal(x, y)
})

test_that("show_all(providers) works for Austria", {
  skip_if_offline()
  x <- show_all(providers)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "providers") |> collect()
  expect_equal(x, y)
})

test_that("show_all(reasons) works for Austria", {
  skip_if_offline()
  x <- show_all(reasons)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "reasons") |> collect()
  expect_equal(x, y)
})

test_that("show_all(profiles) fails for Austria", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) works for Austria", {
  skip_if_offline()
  x <- show_all(lists)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "lists") |> collect()
  expect_equal(x, y)
})

test_that("search_all(fields) works for Austria", {
  skip_if_offline()
  x <- search_all(fields, "year")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Austria", {
  skip_if_offline()
  x <- search_all(taxa, "Vulpes vulpes")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Austria", {
  skip_if_offline()
  x <- search_all(fields, "basis_of_record") |> 
    show_values()
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for lists for Austria", {
  skip_if_offline()
  x <- search_all(lists, "dr113") |> 
    show_values()
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works for Austria", {
  skip_if_offline()
  expect_gt(atlas_counts()$count, 0)
  expect_gt(atlas_counts(type = "species")$count, 0)
})

## FIXME: Only works when run_checks = TRUE
test_that("atlas_counts works with galah_identify for Austria", {
  skip_if_offline()
  result <- galah_call() |>
    galah_identify("Mammalia") |> # run_checks = TRUE works
    atlas_counts()
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    galah_filter(class == "Mammalia") |>
    atlas_counts()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})

test_that("atlas_counts works with group_by for Austria", {
  skip_if_offline()
  result <- galah_call() |>
    galah_filter(year >= 2020) |>
    galah_group_by(year) |>
    atlas_counts()
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

## FIXME: Test only works when run_checks = TRUE
test_that("atlas_occurrences works for Austria", {
  skip_if_offline()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au", 
    download_reason_id = "testing",
    run_checks = TRUE, ## FIXME: Test only works when run_checks = TRUE
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year == 1990) |>
    galah_select(species, year) |>
    atlas_occurrences()   
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})


## FIXME: atlas_taxonomy doesn't work
test_that("atlas_taxonomy works for Austria", {
  skip_if_offline()
  
  ## FIXME: Obsolete syntax. Necessary test? ------------
  # first test child values lookup
  # taxon <- search_taxa("Reptilia")
  # x <- request_values() |>
    # filter(taxa == taxon$taxon_concept_id) |> # should be able to replace this with `identify()`
    # collect()
  # Note that this maxes out at 1000 rows. Clearly, there are two problems here:
  # many taxa missing levels of their taxonomic hierarchy
  # lack of pagination in `galah`
  # add tests
  # expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  # expect_gte(nrow(x), 10)
  # expect_equal(ncol(x), 8)
  # now test if recursive children works via `atlas_taxonomy()`
  # ------------------------------
  
  y <- galah_call() |>
    identify("Aves") |>
    filter(rank >= order) |>
    atlas_taxonomy()
  # add tests
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(y), 
               c("name", "rank", "parent_taxon_concept_id", "taxon_concept_id"))
  expect_gte(nrow(y), 5)
})

galah_config(atlas = "Australia")