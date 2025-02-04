# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Spain works", {
  expect_message(galah_config(atlas = "Spain"))
})

test_that("show_all(fields) works for Spain", {
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

test_that("show_all(licences) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(licences) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(collections) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(collections, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(datasets) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(datasets, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(providers) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(providers, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(reasons) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(reasons) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(assertions) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(assertions) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(profiles) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(lists) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(lists, limit = 10) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_lte(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(fields) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "year") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, "Mammalia") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(taxa) works using data.frames for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, 
                  data.frame(kingdom = "Animalia", 
                             phylum = "Chordata")) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(identifiers) works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(identifiers, "359") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for fields for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(fields, "basisOfRecord") |> 
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for profiles for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(profiles, "LA") |> 
    show_values() |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gte(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_counts works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts() |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with type = 'species' for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- atlas_counts(type = "species") |>
    pull(count) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_gt(x, 0)
})

test_that("atlas_counts works with galah_identify for Spain", {
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

test_that("atlas_counts works with apply_profile for Spain", {
  skip_if_offline(); skip_on_ci()
  without_profile <- galah_call() |>
    count() |>
    collect()
  with_profile <- galah_call() |>
    apply_profile(LA) |>
    count() |>
    collect()
  expect_gt(with_profile$count, 0)
  expect_equal(class(without_profile), class(with_profile))
  expect_lt(with_profile$count, without_profile$count)
})

test_that("atlas_counts works with group_by for Spain", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    filter(year >= 2000) |>
    group_by(basis_of_record) |>
    count() |>
    collect() |>
    try(silent = TRUE)
  skip_if(inherits(result, "try-error"), message = "API not available")
  expect_gt(nrow(result), 1)
  expect_equal(names(result), c("basis_of_record", "count"))
  })

test_that("atlas_counts works with apply_profile for Spain", {
  skip_if_offline(); skip_on_ci()
  without_profile <- galah_call() |>
    count() |>
    collect()
  with_profile <- galah_call() |>
    apply_profile(LA) |>
    count() |>
    collect()
  expect_gt(with_profile$count, 0)
  expect_equal(class(without_profile), class(with_profile))
  expect_lt(with_profile$count, without_profile$count)
})

test_that("atlas_species works for Spain", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Spain",
    email = "test@ala.org.au",
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

test_that("galah_select works for Spain", {
  skip_if_offline(); skip_on_ci()
  x <- galah_select()
  y <- galah_select(basisOfRecord)
  expect_equal(length(x), 2)
  expect_equal(x$summary, "group = basic")
  expect_equal(x$group, "basic")
  expect_true(inherits(x, c("list"))) 
  expect_equal(length(y), 3)
  expect_equal(y$summary, "basisOfRecord")
  expect_equal(y$group, character(0))
  expect_true(inherits(y, c("list"))) 
  expect_true(inherits(y[[1]], c("quosure", "formula"))) 
})

test_that("atlas_occurrences works for Spain", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Spain",
    email = "test@ala.org.au",
    download_reason_id = 10,
    send_email = FALSE)
  occ <- galah_call() |>
    identify("Mammalia") |>
    filter(year <= 1800) |>
    select(species, year) |>
    atlas_occurrences() |>
    try(silent = TRUE)
  skip_if(inherits(occ, "try-error"), message = "API not available")
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_true(inherits(occ, c("tbl_df", "tbl", "data.frame")))
})

test_that("atlas_media() works for Spain", {
  skip_if_offline(); skip_on_ci()
  galah_config(
    atlas = "Spain",
    email = "test@ala.org.au",
    download_reason_id = 10,
    directory = "temp",
    send_email = FALSE)
  x <- request_data() |>
    identify("Mammalia") |>
    filter(year >= 2023
           # imageIDsCount > 0
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
