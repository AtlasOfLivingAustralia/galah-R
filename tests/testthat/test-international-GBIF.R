# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = GBIF works", {
  expect_message(galah_config(atlas = "GBIF",
                              username = "atlasoflivingaustralia",
                              email = "ala4r@ala.org.au",
                              password = "galah-gbif-test-login"))
})

test_that("show_all(fields) works for GBIF", {
  x <- request_metadata() |> collapse()
  expect_true(inherits(x, "query_set"))
  expect_true(x[[1]]$type == "metadata/fields")
  x <- collect(x)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  y <- show_all(fields)
  expect_equal(x, y)
})

test_that("show_all(collections) works for GBIF", {
  skip_if_offline()
  x <- show_all(collections, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  ## FIXME: not coded slice_head() yet
  # y <- request_metadata(type = "collections") |>
  #   slice_head(n = 10) |>
  #   collect()
  # expect_equal(x, y)
  z <- request_metadata() |>
    filter(collection == "australia") |>
    collect()
  expect_equal(nrow(z), 20)
})

test_that("show_all(datasets) works for GBIF", {
  skip_if_offline()
  x <- show_all(datasets, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  z <- request_metadata() |>
    filter(dataset == "avian") |>
    collect()
  expect_equal(nrow(z), 20)
})

test_that("show_all(providers) works for GBIF", {
  skip_if_offline()
  x <- show_all(providers, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  z <- request_metadata() |>
    filter(provider == "herbarium") |>
    collect()
  expect_equal(nrow(z), 20)
})

test_that("show_all(reasons) fails for GBIF", {
  expect_error(show_all(reasons))
})

test_that("show_all(assertions) works for GBIF", {
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_all(profiles) fails for GBIF", {
  expect_error(show_all(profiles))
})

test_that("show_all(lists) fails for GBIF", {
  expect_error(show_all(lists))
})

test_that("search_all(taxa) works for GBIF", {
  skip_if_offline()
  x <- search_taxa("Mammalia")
  expect_equal(nrow(x), 1)
  expect_gte(ncol(x), 1)
  expect_true(colnames(x)[1] == "search_term")
  expect_true(x$class == "Mammalia")
})

galah_config(verbose = TRUE)

test_that("search_all(datasets) works for GBIF", {
  skip_if_offline()
  x <- search_all(datasets, "Mammals")
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(collections) works for GBIF", {
  skip_if_offline()
  x <- search_all(collections, "Museum")
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("search_all(providers) works for GBIF", {
  skip_if_offline()
  x <- search_all(providers, "Frog")
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

galah_config(verbose = FALSE)

test_that("search_all(fields) works for GBIF", {
  skip_if_offline()
  result <- search_all(fields, "year")
  expect_equal(nrow(result), 2)
  expect_true(inherits(result, c("tbl_df", "tbl", "data.frame")))
})

test_that("show_values works for GBIF fields", {
  skip_if_offline()
  search_fields("basisOfRecord") |>
    show_values() |>
    nrow() |>
    expect_gt(1)
})

test_that("atlas_counts works for GBIF", {
  skip_if_offline()
  expect_gt(atlas_counts()$count, 0)
})

test_that("atlas_counts fails for GBIF when type = 'species'", {
  expect_error(atlas_counts(type = "species"))
})

galah_config(run_checks = TRUE)

test_that("`count()` works with `filter()` for GBIF", {
  skip_if_offline()
  # collapse
  x <- request_data() |>
    filter(year == 2010) |>
    count() |>
    collapse()
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 3)
  expect_equal(
    c(x[[1]]$type, x[[2]]$type, x[[3]]$type),
    c("metadata/fields", 
      "metadata/assertions", 
      "data/occurrences-count"))
  # compute
  y <- compute(x)
  expect_s3_class(y, "query")
  expect_equal(length(y), 5)
  expect_equal(names(y), c("type",
                           "url",
                           "slot_name",
                           "expand",
                           "headers"))
  # collect
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gt(z$count, 1)
  expect_equal(nrow(z), 1)
})

test_that("`count` works with `identify` for GBIF", {
  skip_if_offline()
  # collapse
  x <- request_data() |>
    identify("Mammalia") |>
    count() |>
    collapse()
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 2)
  expect_equal(
    c(x[[1]]$type, x[[2]]$type),
    c("metadata/taxa-single", "data/occurrences-count"))
  # compute
  y <- compute(x)
  expect_s3_class(y, "query")
  expect_equal(length(y), 5)
  expect_equal(names(y), c("type",
                           "url",
                           "slot_name",
                           "expand",
                           "headers"))
  # collect
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gt(z$count, 1)
  expect_equal(nrow(z), 1)
})

test_that("`count` works with `group_by` for GBIF", {
  skip_if_offline()
  x <- galah_call() |>
    identify("Litoria") |>
    filter(year >= 2020) |>
    group_by(year) |>
    count() |>
    collapse()
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 4)
  expect_equal(
    unlist(lapply(x, function(a){a$type})),
    c("metadata/fields", 
      "metadata/assertions",
      "metadata/taxa-single",
      "data/occurrences-count-groupby"))
  # compute
  y <- compute(x)
  expect_s3_class(y, "query")
  expect_equal(length(y), 4)
  expect_equal(names(y), c("type",
                           "url",
                           "expand",
                           "headers"))
  # collect
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(z), 1)
  expect_equal(names(z), c("year", "count"))
})

# FIXME: GBIF grouped counts only work for n = 1 - expand this or add warning
# FIXME: `slice_head()` not tested for GBIF
# FIXME: `check_fields()` not tested for GBIF - try sending invalid fields to `filter()`

test_that("`galah_select()` returns message for GBIF", {
  expect_message({x <- galah_select(galah_call())})
  expect_true(is.null(x$select))
})

test_that("atlas_species works for GBIF", {
  x <- request_data(type = "species") |>
    filter(year == 2010) |>
    identify("Litoria") |>
    collapse()
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 4)
  expect_equal(
    unlist(lapply(x, function(a){a$type})),
    c("metadata/fields", 
      "metadata/assertions",
      # "metadata/reasons",
      "metadata/taxa-single",
      "data/species"))  
  skip_if_offline()
  y <- compute(x)
  expect_true(inherits(y, "query"))
  expect_true(y$type == "data/species")  
  z <- collect(y)
  expect_gt(nrow(z), 0)
  expect_gt(ncol(z), 0)
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
  species <- galah_call() |>
    galah_filter(year == 2010) |>
    galah_identify("Litoria") |>
    atlas_species()
  expect_equal(z, species)
})

test_that("atlas_media fails for GBIF", {
  skip_if_offline()
  expect_error({galah_call() |>
    galah_identify("perameles") |>
    atlas_media()
  })
})

test_that("`collapse()` et al. work for GBIF with `type = 'occurrences'`", {
  skip_if_offline()
  # collapse
  base_query <- request_data() |>
    identify("Vulpes vulpes") |>
    filter(year == 1900)
  count <- base_query |> 
    count() |> 
    collect()
  x <- base_query |>
    collapse()
  # NOTE: the above query should return 147 records (tested 2023-10-12)
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 4)
  expect_equal(
    unlist(lapply(x, function(a){a$type})),
    c("metadata/fields", 
      "metadata/assertions",
      # "metadata/reasons",
      "metadata/taxa-single",
      "data/occurrences"))
  # compute
  y <- compute(x)
  expect_true(inherits(y, "query"))
  expect_true(y$type == "data/occurrences")  
  expect_true(any(names(y) == "status"))
  # collect
  z <- collect(y)
  expect_gt(nrow(z), 0)
  expect_gt(ncol(z), 0)
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
  expect_equal(nrow(z), count$count)
  citation <- atlas_citation(z)
  expect_true(grepl("^GBIF.org", citation))
})

galah_config(atlas = "Australia")