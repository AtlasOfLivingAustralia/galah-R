# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Flanders works", {
  expect_message(galah_config(atlas = "Flanders"))
})

test_that("show_all(fields) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(fields) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("show_all(licences) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(licences) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(profiles) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  
  # and values
  y <- request_metadata() |>
    filter(profiles == x$short_name[1]) |>
    unnest() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(y), 1)
  expect_true(inherits(y, c("tbl_df", "tbl", "data.frame")))
  
  # and actually reduces record count
  records_all <- galah_call() |>
    count() |>
    collect()
  records_clean <- galah_call() |>
    apply_profile(x$short_name[1]) |>
    count() |>
    collect()
  expect_lt(records_clean$count, records_all$count)
})

test_that("show_all(lists) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(lists, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`unnest()` works for lists in Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- request_metadata() |>
    filter(list == "dr565") |>
    unnest() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_true(any(colnames(x) == "taxon_concept_id"))
})

test_that("search_all(identifiers) works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(identifiers, "359") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Flanders", {
  skip_if_offline(); skip_on_ci()
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  x <- search_all(fields, "basisOfRecord") |>
    quiet_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts() |>
    dplyr::pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts(type = "species") |>
    dplyr::pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("`count()` works with galah_identify for Flanders", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(result$count, 1)
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

test_that("`glimpse()` works for Flanders", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    filter(year == 2025) |>
    glimpse() |>
    collect()
  expect_s3_class(x, c("occurrences_glimpse", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3) # number of rows in the tibble
  quiet_print <- purrr::quietly(print.occurrences_glimpse)
  x_print <- strsplit(quiet_print(x)$output, "\n")[[1]] # print statement
  expect_gt(length(x_print), 5)
  stringr::str_detect(x_print,
                       "^\\$ (taxonConceptID|eventDate|decimalLatitude|scientificName)") |>
    which() |>
    length() |>
    expect_equal(4) # note: recordID missing
})

test_that("`count()` works with `group_by()` for Flanders", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    filter(year >= 2000) |>
    group_by(basisOfRecord) |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("basisOfRecord", "count"))
  })

test_that("`atlas_species()` works for Flanders", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Flanders",
    email = "galah@natuurdata@inbo.be",
    send_email = FALSE)
  spp <- galah_call() |>
    identify("Corvus") |>
    atlas_species() |>
    try(silent = TRUE)
  skip_if(inherits(spp, "try-error"), message = "API not available")
  expect_gt(nrow(spp), 1)
  expect_equal(ncol(spp), 11)
  expect_s3_class(spp, c("tbl_df", "tbl", "data.frame"))
})

test_that("`atlas_occurrences()` works for Flanders", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Flanders",
    email = "galah.natuurdata@inbo.be",
    download_reason_id = 10,
    send_email = FALSE)
  query <- galah_call() |>
    identify("Canis") |>
    filter(year == 2020) |>
    select(species, year) |>
    collapse()
  
  response <- compute(query) |>
    try(silent = TRUE)
  skip_if(inherits(response, "try-error"), message = "API not available")
  
  occ <- collect(response) |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

test_that("`atlas_media()` works for Flanders", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Flanders",
    email = "test@ala.org.au",
    download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    identify("Vulpes") |>
    filter(year == 2025,
           basisOfRecord == "HUMAN_OBSERVATION",
           # month == 6,
           !is.na(images)
           ) |>
    # group_by(year) |>
    # count() |>
    # collect()
    atlas_media() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
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