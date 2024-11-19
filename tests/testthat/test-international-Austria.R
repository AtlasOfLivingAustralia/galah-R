# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Austria works", {
  expect_message(galah_config(atlas = "Austria"))
})

test_that("show_all(assertions) works for Austria", {
  skip_if_offline()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "assertions") |> collect()
  expect_equal(x, y)
})

test_that("show_all(collections) works for Austria", {
  skip_if_offline()
  x <- show_all(collections) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "collections") |> collect()
  expect_equal(x, y)
})

test_that("show_all(datasets) works for Austria", {
  skip_if_offline()
  x <- show_all(datasets) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "datasets") |> collect()
  expect_equal(x, y)
})

test_that("show_all(fields) works for Austria", {
  skip_if_offline()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "fields") |> collect()
  expect_equal(x, y)
})

test_that("show_all(licences) works for Austria", {
  skip_if_offline()
  x <- show_all(licences) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  # this API exists, but is empty at time of writing (2024-02-26)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "licences") |> collect()
  expect_equal(x, y)
})

test_that("show_all(lists) works for Austria", {
  skip_if_offline()
  x <- show_all(lists) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "lists") |> collect()
  expect_equal(x, y)
})

test_that("show_all(providers) works for Austria", {
  skip_if_offline()
  x <- show_all(providers) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "providers") |> collect()
  expect_equal(x, y)
})

test_that("show_all(reasons) works for Austria", {
  skip_if_offline()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- request_metadata(type = "reasons") |> collect()
  expect_equal(x, y)
})

test_that("show_all(profiles) fails for Austria", {
  expect_error(show_all(profiles))
})

test_that("search_all(fields) works for Austria", {
  skip_if_offline()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Austria", {
  skip_if_offline()
  x <- search_all(taxa, "Vulpes vulpes") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Austria", {
  skip_if_offline()
  x <- try({search_all(fields, "basis_of_record") |> 
    show_values()},
    silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for lists for Austria", {
  skip_if_offline()
  x <- try({search_all(lists, "dr30") |> 
    show_values()},
    silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works with type = 'occurrences' for Austria", {
  skip_if_offline()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Austria", {
  skip_if_offline()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

## FIXME: Only works when run_checks = TRUE
test_that("atlas_counts works with galah_identify for Austria", {
  skip_if_offline()
  result <- galah_call() |>
    galah_identify("Mammalia") |> # run_checks = TRUE works
    atlas_counts() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
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
    atlas_counts() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("atlas_species works for Austria", {
  skip_if_offline()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au", 
    download_reason_id = "testing",
    send_email = FALSE)
  spp <- galah_call(type = "species") |>
    identify("Mammalia") |> # NOTE: testing with Reptilia failed as this taxon not uniquely matched
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(spp, "try-error"), message = "API not available")
  skip_if((nrow(spp) < 1 & ncol(spp) < 1), message = "API not available")
  expect_gt(nrow(spp), 100) # actual number 569 spp on 2024-08-22
  expect_gt(ncol(spp), 8) # actually 10
  expect_s3_class(spp, c("tbl_df", "tbl", "data.frame"))
})

## FIXME: Test only works when run_checks = TRUE
test_that("atlas_occurrences works for Austria", {
  skip_if_offline()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au", 
    directory = "temp",
    download_reason_id = 10,
    run_checks = TRUE, ## FIXME: Test only works when run_checks = TRUE
    send_email = FALSE)
  base_query <- galah_call() |>
    identify("Mammalia") |>
    filter(year == 1990) 
  counts <- base_query |>
    count() |>
    collect()
  occ_collapse <- base_query |>
    select(species, year) |>
    collapse() |>
    try(silent = TRUE)
  skip_if(inherits(occ_collapse, "try-error"), message = "API not available")
  expect_s3_class(occ_collapse, "query")
  expect_equal(names(occ_collapse), 
               c("type", "url", "headers", "filter"))
  expect_equal(occ_collapse$type, "data/occurrences")
  # compute
  occ_compute <- compute(occ_collapse)
  expect_s3_class(occ_compute, "computed_query")
  # collect
  occ <- collect(occ_compute) |>
    try(silent = TRUE)
  skip_if(inherits(occ_compute, "try-error"), message = "API not available")
  expect_equal(nrow(occ), counts$count[1])
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(occ), 2)
  unlink("temp", recursive = TRUE)
})

test_that("atlas_media() works for Austria", {
  skip_if_offline()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au",
    download_reason_id = "testing",
    directory = "temp",
    run_checks = TRUE,
    send_email = FALSE)
  x <- request_data() |>
    identify("Mammalia") |>
    filter(year == 2010,
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

## FIXME: atlas_taxonomy doesn't work
test_that("atlas_taxonomy works for Austria", {
  skip_if_offline()
  y <- galah_call() |>
    identify("Aves") |>
    filter(rank >= order) |>
    atlas_taxonomy()|>
    try(silent = TRUE)
  skip_if(inherits(y, "try-error"), message = "API not available")
  # add tests
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(y), 
               c("name", "rank", "parent_taxon_concept_id", "taxon_concept_id"))
  expect_gte(nrow(y), 5)
})

galah_config(atlas = "Australia")
