# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Austria works", {
  expect_message(galah_config(atlas = "Austria"))
})

test_that("`show_all(assertions)` works for Austria", {
  skip_galah()
  x <- show_all(assertions) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  y <- request_metadata(type = "assertions") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(collections)` works for Austria", {
  skip_galah()
  x <- show_all(collections) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 10)
  y <- request_metadata(type = "collections") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(datasets)` works for Austria", {
  skip_galah()
  x <- show_all(datasets) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 10)
  y <- request_metadata(type = "datasets") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(fields)` works for Austria", {
  skip_galah()
  x <- show_all(fields) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  y <- request_metadata(type = "fields") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(licences)` works for Austria", {
  # this API exists, but is empty at time of writing (2024-02-26)
  skip_galah()
  x <- show_all(licences) |>
    skip_unavailable()
  expect_tibble(x)
  expect_lte(nrow(x), 10)
  y <- request_metadata(type = "licences") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(lists)` works for Austria", {
  skip_galah()
  x <- show_all(lists) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  y <- request_metadata(type = "lists") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(providers)` works for Austria", {
  skip_galah()
  x <- show_all(providers) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  y <- request_metadata(type = "providers") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(reasons)` works for Austria", {
  skip_galah()
  x <- show_all(reasons) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  y <- request_metadata(type = "reasons") |> collect()
  expect_equal(x, y)
})

test_that("`show_all(profiles)` fails for Austria", {
  expect_error(show_all(profiles))
})

test_that("`search_all(fields)` works for Austria", {
  skip_galah()
  x <- search_all(fields, "year") |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 1)
})

test_that("`search_all(taxa)` works for Austria", {
  skip_galah()
  x <- search_all(taxa, "Vulpes vulpes") |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 1)
})

test_that("`show_values()` works for Austria", {
  skip_galah()
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  # fields
  x <- search_all(fields, "basis_of_record") |> 
    quiet_values() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 1)

  # lists
  x <- search_all(lists, "dr30") |> 
    quiet_values() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 1)
})

test_that("`atlas_counts()` works with type = 'occurrences' for Austria", {
  skip_galah()
  x <- atlas_counts() |>
    skip_unavailable()
  expect_tibble(x)
  x |>
    dplyr::pull(count) |>
    expect_gt(0)
})

test_that("`atlas_counts()` works with type = 'species' for Austria", {
  skip_galah()
  x <- atlas_counts(type = "species") |>
    skip_unavailable()
  expect_tibble(x)
  x |>
    dplyr::pull(count) |>
    expect_gt(0)
})

## FIXME: Only works when run_checks = TRUE
test_that("`count()` works with `identify()` for Austria", {
  skip_galah()
  result <- galah_call(from = "Austria") |>
    identify("Mammalia") |> # run_checks = TRUE works
    count() |>
    collect() |>
    skip_unavailable()
  expect_tibble(result)
  expect_gt(result$count, 1)
  result2 <- galah_call() |>
    galah_filter(class == "Mammalia") |>
    atlas_counts()
  expect_lt(
    sqrt((result2$count - result$count)^2) / result$count, 
    0.1) # i.e. <1% margin of error
})

test_that("`count()` works with group_by for Austria", {
  skip_galah()
  result <- galah_call(from = "Austria") |>
    filter(year >= 2020) |>
    count(year) |>
    collect() |>
    skip_unavailable()
  expect_tibble(result)
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("year", "count"))
})

test_that("`atlas_species()` works for Austria", {
  skip_galah()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au", 
    download_reason_id = "testing",
    send_email = FALSE)
  spp <- galah_call(type = "species") |>
    identify("Mammalia") |> # NOTE: testing with Reptilia failed as this taxon not uniquely matched
    collect() |>
    skip_unavailable()
  expect_tibble(spp)
  skip_if((nrow(spp) < 1 & ncol(spp) < 1), message = "API not available")
  expect_gt(nrow(spp), 100) # actual number 569 spp on 2024-08-22
  expect_gt(ncol(spp), 8) # actually 10
})

## FIXME: Test only works when run_checks = TRUE
test_that("`atlas_occurrences()` works for Austria", {
  skip_galah()
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
    skip_unavailable()
  expect_s3_class(occ_collapse, "query")
  expect_equal(names(occ_collapse), 
               c("type", "atlas", "url", "headers", "request"))
  expect_equal(occ_collapse$type, "data/occurrences")
  # compute
  occ_compute <- compute(occ_collapse) |>
    skip_unavailable()
  expect_s3_class(occ_compute, "computed_query")
  # collect with pause of 20 seconds and `wait = FALSE`
  # This is clumsy, but useful beacuse previously we were getting wait times of ~10 minutes
  Sys.sleep(20)
  occ <- collect(occ_compute, wait = FALSE) |>
    skip_unavailable(other_classes = "computed_query") 
  expect_tibble(occ)
  expect_equal(nrow(occ), counts$count[1])
  expect_tibble(occ)
  expect_equal(ncol(occ), 2)
  unlink("temp", recursive = TRUE)
})

test_that("`atlas_media()` works for Austria", {
  skip_galah()
  galah_config(
    atlas = "Austria",
    email = "ala4r@ala.org.au",
    download_reason_id = "testing",
    directory = "temp",
    run_checks = TRUE,
    send_email = FALSE)
  x <- request_data() |>
    identify("Mammalia") |>
    filter(year == 2010) |>
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
  n_downloads <- 3
  quiet_media(x[seq_len(n_downloads), ])
  expect_equal(length(list.files("temp", pattern = ".jpg$")),
               n_downloads)
  unlink("temp", recursive = TRUE)
})

test_that("atlas_taxonomy works for Austria", {
  skip_galah()
  x <- galah_call() |>
    identify("Aves") |>
    filter(rank >= order) |>
    atlas_taxonomy() |>
    skip_unavailable()
  expect_tibble(x)
  expect_equal(colnames(x), 
               c("name", "rank", "parent_taxon_concept_id", "taxon_concept_id"))
  expect_gte(nrow(x), 5)
})

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config)