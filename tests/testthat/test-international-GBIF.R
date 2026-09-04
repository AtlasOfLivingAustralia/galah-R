# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

# This script includes a delay between successive GBIF occurrence queries.
# This is needed because GBIF appears to limit the number of queries allowed
# per unit time. The delay (in seconds) is set here:
pause_time <- 2

test_that("swapping to atlas = GBIF works", {
  expect_message(galah_config(atlas = "GBIF",
                              username = "atlasoflivingaustralia",
                              email = "ala4r@ala.org.au",
                              password = "galah-gbif-test-login"))
})

test_that("`show_all(fields)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  # first ensure underlying syntax is valid
  x <- request_metadata() |> 
    collapse()
  expect_true(inherits(x, "query"))
  expect_true(x$type == "metadata/fields")
  x <- collect(x)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # then test 'traditional' syntax
  y <- show_all(fields)
  expect_equal(x, y)
})

test_that("`search_all(fields)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  result <- search_all(fields, "year")
  expect_gte(nrow(result), 2)
  result |> 
    inherits(c("tbl_df", "tbl", "data.frame")) |>
    expect_true()
  grepl("year", tolower(result$id)) |>
    all() |>
    expect_true()
})

test_that("`show_values()` works for GBIF fields", {
  skip_if_offline(); skip_on_ci()
  galah_config(atlas = "GBIF")
  # query syntax
  x <- request_metadata() |>
    filter(fields == "gbifRegion") |>
    unnest() |>
    collect()
  # traditional syntax
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  y <- search_fields("gbifRegion") |>
    quiet_values()
  # tests
  inherits(x, c("tbl_df", "tbl", "data.frame")) |>
    expect_true()
  x |>
    nrow() |>
    expect_gt(1)
  expect_equal(x, y)
})

test_that("`show_values()` fails for unsearchable GBIF fields", {
  skip_if_offline(); skip_on_ci()
  request_metadata(from = "GBIF") |>
    filter(field == "accessRights") |>
    unnest() |>
    collapse() |>
    expect_error()
})

test_that("`show_all(collections)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
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

test_that("`show_all(datasets)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(datasets, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  z <- request_metadata() |>
    filter(dataset == "avian") |>
    collect()
  expect_gte(nrow(z), 10) # historically this was n = 20
})

test_that("`show_all(providers)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- show_all(providers, limit = 10)
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  z <- request_metadata() |>
    filter(provider == "herbarium") |>
    collect()
  expect_equal(nrow(z), 20)
})

test_that("`show_all(reasons)` fails for GBIF", {
  expect_error(show_all(reasons))
})

test_that("`show_all(assertions)` works for GBIF", {
  x <- show_all(assertions)
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`show_all(profiles)` fails for GBIF", {
  expect_error(show_all(profiles))
})

test_that("`show_all(lists)` fails for GBIF", {
  expect_error(show_all(lists))
})

test_that("`search_all(taxa)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- search_taxa("Mammalia")
  expect_equal(nrow(x), 1)
  expect_gte(ncol(x), 1)
  expect_true(colnames(x)[1] == "search_term")
  expect_true(x$class == "Mammalia")
})

test_that("`search_all(taxa)` works using a tibble for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(taxa, 
                  data.frame(kingdom = "Animalia", 
                             phylum = "Chordata")) |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_equal(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`search_all(identifiers)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(identifiers, "359") |>
    try(silent = TRUE)
  skip_if(inherits(x, "try-error"), message = "API not available")
  expect_equal(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

galah_config(verbose = TRUE)

test_that("`search_all(datasets)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(datasets, "Mammals")
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`search_all(collections)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(collections, "Museum")
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`search_all(providers)` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  x <- search_all(providers, "Frog")
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

galah_config(verbose = FALSE)

test_that("`count()` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  galah_call() |>
    count() |>
    collect() |>
    dplyr::pull("count") |>
    expect_gt(0)
})

test_that("`atlas_counts()` fails for GBIF when `type = 'species'`", {
  expect_error(atlas_counts(type = "species"))
})

galah_config(run_checks = TRUE)

test_that("`count()` works with 2 `group_by` args for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  x <- galah_call() |>
    filter(year >= 2020) |>
    group_by(year, basisOfRecord) |>
    count() |>
    collapse()
  expect_s3_class(x, "query")
  # expect_equal(length(x), 6) # for some reason this fails on Positron?!?!?!!
  expect_contains(names(x), 
               c("type", "url", "headers", "request")) # "options", "body" ?
  expect_equal(x$type, "data/occurrences-count-groupby")
  # compute
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  # collect
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(z), 1)
  expect_equal(names(z), c("year", "basisOfRecord", "count"))
  # in early versions, iterating by a variable wasn't working properly
  # check that same level of second variable differs across first variable
  z_counts <- z |>
    dplyr::filter(basisOfRecord == "HUMAN_OBSERVATION") |>
    dplyr::pull(count)
  expect_true(max(z_counts) - min(z_counts) > 0)
})

## group_by fails when an invalid field is given
## NOTE: fails: no checks run at present
# expect_error({
#   galah_call() |>
#     identify("Crinia") |>
#     group_by(species) |>
#     count() |>
#     collect()
# })

# FIXME: `slice_head()` not tested for GBIF

test_that("`count()` works with `identify` for GBIF when `run_checks` = TRUE", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  galah_config(run_checks = TRUE)
  # collapse
  x <- request_data() |>
    identify("Litoria peronii") |>
    filter(year >= 2020, 
           basisOfRecord == "HUMAN_OBSERVATION") |>
    count() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(length(x), 7)
  expect_equal(names(x), 
               c("type", "atlas", "url", "headers",
                 "options", "body", "request"))
  expect_equal(x$type, "data/occurrences-count")
  # compute
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  # collect
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gt(z$count, 1)
  expect_equal(nrow(z), 1)
})

test_that("`glimpse()` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  galah_config(atlas = "GBIF",
              username = "atlasoflivingaustralia",
              email = "ala4r@ala.org.au",
              password = "galah-gbif-test-login")
  x <- request_data(from = "GBIF") |>
    filter(year == 2025) |>
    glimpse() |>
    collect()
  expect_s3_class(x, c("occurrences_glimpse", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3) # number of rows in the tibble
  quiet_print <- purrr::quietly(print.occurrences_glimpse)
  x_print <- strsplit(quiet_print(x)$output, "\n")[[1]] # print statement
  expect_gt(length(x_print), 5)
  stringr::str_detect(x_print,
                       "^\\$ (taxonConceptID|eventDate|decimalLatitude)") |>
    which() |>
    length() |>
    expect_equal(3)
})

test_that("`glimpse()` works with `identify()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  galah_config(atlas = "GBIF",
              username = "atlasoflivingaustralia",
              email = "ala4r@ala.org.au",
              password = "galah-gbif-test-login")
  x <- request_data(from = "GBIF") |>
    identify("Eolophus roseicapilla") |>
    filter(year == 2010) |>
    glimpse() |>
    collect()
  expect_s3_class(x, c("occurrences_glimpse", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3) # number of rows in the tibble
  quiet_print <- purrr::quietly(print.occurrences_glimpse)
  x_print <- strsplit(quiet_print(x)$output, "\n")[[1]] # print statement
  expect_gt(length(x_print), 5)
  stringr::str_detect(x_print,
                       "^\\$ (taxonConceptID|eventDate|decimalLatitude)") |>
    which() |>
    length() |>
    expect_equal(3)
})

# FIXME: add test for `select()` paired with `glimpse()`

test_that("`atlas_occurrences()` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  x <- galah_call(from = "GBIF") |>
    filter(year == 1890,
           classKey == "359",
           country == "AU") |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 10)
})

test_that("`atlas_occurrences()` works with `select()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  x <- galah_call(from = "GBIF") |>
    filter(year == 1890,
           classKey == "359",
           country == "AU") |>
    select(countryCode, species, eventDate) |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(x), 10)
  expect_equal(ncol(x), 3)
})

test_that("invalid fields are caught from `select()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  galah_call(from = "GBIF") |>
    filter(year == 1890,
           classKey == "359",
           country == "AU") |> # this uses `check_fields_gbif_predicates()`
    select(country, species, eventDate) |> # this uses `check_fields_gbif_counts()`
    collect() |>
    expect_error()
})

test_that("`atlas_species()` works for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  x <- request_data(type = "species",
                    from = "GBIF") |>
    authenticate(username = "atlasoflivingaustralia",
                 email = "ala4r@ala.org.au",
                 password = "galah-gbif-test-login") |>
    filter(year == 2010) |>
    identify("Crinia") |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(length(x), 7)
  expect_equal(names(x), 
               c("type", "atlas", "url", "headers", "options", "body", "request"))
  expect_equal(x$type, "data/species")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_gt(nrow(z), 10) # n = 14
  expect_gt(ncol(z), 5)  # n = 22
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
})

test_that("`count()` queries work with `authenticate()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  x <- request_data(from = "GBIF") |>
    authenticate(username = "atlasoflivingaustralia",
                 email = "ala4r@ala.org.au",
                 password = "galah-gbif-test-login") |>
    filter(year == 2010) |>
    count() |>
    collect()
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`distinct()` queries accept `select()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  x <- request_data(from = "GBIF") |>
    authenticate(username = "atlasoflivingaustralia",
                 email = "ala4r@ala.org.au",
                 password = "galah-gbif-test-login") |>
    filter(year == 2010) |>
    distinct(species_guid, .keep_all = TRUE) |>
    identify("Crinia") |>
    select(scientificName, speciesKey) |>
    collect()
  expect_gt(nrow(x), 10) # 
  expect_equal(ncol(x), 2)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`atlas_media()` fails for GBIF", {
  skip_if_offline(); skip_on_ci()
  expect_error({galah_call() |>
    identify("perameles") |>
    atlas_media()
  })
})

test_that("`collect()` works for GBIF with `type = 'occurrences' or 'occurrences-doi'` ", {
  skip_if_offline(); skip_on_ci()
  Sys.sleep(pause_time)
  # collapse
  base_query <- request_data() |>
    identify("Vulpes vulpes") |>
    filter(year == 1900)
  count <- base_query |> 
    count() |> 
    collect()
  Sys.sleep(pause_time)
  x <- base_query |>
    collapse()
  # NOTE: the above query should return 72 records (tested 2025-06-10)
  expect_s3_class(x, "query")
  expect_equal(names(x), 
               c("type", "atlas", "url", "headers", "options", "body", "request"))
  expect_equal(x$type, "data/occurrences")
  # compute
  y <- compute(x)
  expect_true(inherits(y, "computed_query"))
  expect_true(y$type == "data/occurrences")  
  expect_true(any(names(y) == "status"))
  # collect
  z <- collect(y)
  expect_gt(nrow(z), 0)
  expect_gt(ncol(z), 0)
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
  expect_equal(nrow(z), count$count)
  expect_true(!is.null(attributes(z)$doi))

  # FIXME: need DOI search test
  Sys.sleep(pause_time)
  recent_doi <- attributes(z)$doi
  a <- galah_call() |>
   filter(doi == recent_doi) |>
   collect()
  expect_equal(a, z, ignore_attr = TRUE)
  expect_equal(attributes(a)$doi, attributes(z)$doi)
})

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config, pause_time)