# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Sweden works", {
  expect_message(galah_config(atlas = "Sweden"))
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

test_that("show_all(fields) works for Sweden", {
  skip_if_offline()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(licences) works for Sweden", {
  skip_if_offline()
  x <- show_all(licences, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  # this API exists, but is empty at time of writing (2024-02-26)
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(lists) works for Sweden", {
  skip_if_offline()
  x <- show_all(lists, limit = 10) |>
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

test_that("show_all(profiles) works for Sweden", {
  skip_if_offline()
  x <- show_all(profiles) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
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

test_that("show_values works fields in Sweden", {
  skip_if_offline()
  x <- search_fields("basisOfRecord") |>
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
})

test_that("show_values works for lists in Sweden", {
  skip_if_offline()
  x <- try({search_all(lists, "dr156") |> 
      show_values()},
      silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for profiles in Sweden", {
  skip_if_offline()
  x <- try({search_all(profiles, "SBDI") |> 
      show_values()},
      silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
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

test_that("atlas_counts works with apply_profile for Sweden", {
  skip_if_offline()
  without_profile <- galah_call() |>
    count() |>
    collect()
  with_profile <- galah_call() |>
    apply_profile(SBDI) |>
    count() |>
    collect()
  expect_gt(with_profile$count, 0)
  expect_equal(class(without_profile), class(with_profile))
  expect_lt(with_profile$count, without_profile$count)
})

test_that("atlas_species works for Sweden", {
  skip_if_offline()
  galah_config(
    atlas = "Sweden",
    email = "martinjwestgate@gmail.com",
    send_email = FALSE)
  spp <- galah_call() |>
    galah_identify("Carnivora") |>
    atlas_species() |>
    try(silent = TRUE)
  skip_if(inherits(spp, "try-error"), message = "API not available")
  expect_gt(nrow(spp), 20) # actual number 105 spp on 2024-03-22
  expect_gte(ncol(spp), 10) # actual number 11 cols on 2024-04-12
  expect_s3_class(spp, c("tbl_df", "tbl", "data.frame"))
})

test_that("atlas_occurrences works for Sweden", {
  skip_if_offline()
  galah_config(
    atlas = "Sweden",
    email = "martinjwestgate@gmail.com",
    send_email = FALSE)
  occ_collapse <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year < 1850) |>
    galah_select(group = "basic") |> # use defaults
    collapse()
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
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(occ), length(default_columns()))
})

test_that("atlas_media() works for Sweden", {
  skip_if_offline()
  galah_config(
    atlas = "Sweden",
    email = "martinjwestgate@gmail.com",
    send_email = FALSE)
  x <- request_data() |>
    filter(year == 2010) |>
    select(group = c("basic", "media")) |>
    identify("Amphibia") |>
    atlas_media() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(x), 1)
  expect_equal(colnames(x)[1:2],
               c("media_id", "recordID"))
})

test_that("collect_media() works for Sweden", {
  skip_if_offline()
  galah_config(
    atlas = "Sweden",
    email = "martinjwestgate@gmail.com",
    send_email = FALSE)
  x <- request_data() |>
    identify("Amphibia") |>
    filter(year == 2010,
           imageIDsCount > 0) # |> # multimediaCount
  # get counts
  media_count <- x |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(media_count, "try-error"), message = "API not available")
  # get occurrences
  media_occ <- x |>
    select(group = c("basic", "media")) |>
    collect(wait = TRUE) |>
    try(silent = TRUE)
  skip_if(inherits(media_occ, "try-error"), message = "API not available")
  # get metadata
  media_meta <- request_metadata() |>
    filter(media == media_occ) |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(media_meta, "try-error"), message = "API not available")
  expect_gt(nrow(media_meta), 0)
  # get files 
  galah_config(directory = "temp")
  n_downloads <- 3
  request_files() |>
    filter(media == media_meta[seq_len(n_downloads), ]) |>
    collect(thumbnail = TRUE)
  expect_equal(length(list.files("temp", pattern = ".jpg$")),
               n_downloads)
  unlink("temp", recursive = TRUE)
  # try with collect_media()
  collect_media(media_meta[seq_len(n_downloads), ])
  expect_equal(length(list.files("temp", pattern = ".jpg$")),
               n_downloads)
  unlink("temp", recursive = TRUE)
})

galah_config(atlas = "Australia")