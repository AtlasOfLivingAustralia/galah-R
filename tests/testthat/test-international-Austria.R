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
  spp <- galah_call() |>
    galah_identify("Carnivora") |>
    atlas_species() |>
    try(silent = TRUE)
  skip_if(inherits(spp, "try-error"), message = "API not available")
  skip_if((nrow(spp) < 1 & ncol(spp) < 1), message = "API not available")
  expect_gt(nrow(spp), 20) # actual number 105 spp on 2024-03-22
  expect_gt(ncol(spp), 8) # actually 10
  expect_s3_class(spp, c("tbl_df", "tbl", "data.frame"))
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
    atlas_occurrences() |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_media() works for Austria", {
  skip_if_offline()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au", 
    download_reason_id = "testing",
    run_checks = TRUE,
    send_email = FALSE)
  x <- request_data() |>
    identify("Mammalia") |>
    filter(!is.na(image_url),
           year == 2010) |>
    # count() |>
    select(record_number, image_url) |>
    collect(wait = TRUE) |>
  # should return 10 occurrences
  # fails rn due to bugs in biocache-service (2024-02-27)
  # stages after this can't be tested until above issue is resolved.
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 0)
  y <- request_metadata() |>
    filter(media == x) |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(y, "try-error"), message = "API not available")
  expect_gt(nrow(y), 0)
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
