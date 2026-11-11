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

## metadata tests

test_that("`show_all(fields)` works for GBIF", {
  skip_galah()
  # first ensure underlying syntax is valid
  x <- request_metadata() |> 
    collapse()
  expect_true(inherits(x, "query"))
  expect_true(x$type == "metadata/fields")
  x <- collect(x) |>
    skip_unavailable()
  expect_gt(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # then test 'traditional' syntax
  y <- show_all(fields) |>
    skip_unavailable()
  expect_equal(x, y)
})

test_that("`search_all(fields)` works for GBIF", {
  skip_galah()
  result <- search_all(fields, "year") |>
    skip_unavailable()
  expect_gte(nrow(result), 2)
  result |> 
    inherits(c("tbl_df", "tbl", "data.frame")) |>
    expect_true()
  grepl("year", tolower(result$id)) |>
    all() |>
    expect_true()
})

test_that("`show_values()` works for GBIF fields", {
  skip_galah()
  galah_config(atlas = "GBIF")
  # query syntax
  x <- request_metadata() |>
    filter(fields == "gbifRegion") |>
    unnest() |>
    collect() |>
    skip_unavailable()
  # traditional syntax
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  y <- search_fields("gbifRegion") |>
    quiet_values()  |>
    skip_unavailable()
  # tests
  inherits(x, c("tbl_df", "tbl", "data.frame")) |>
    expect_true()
  x |>
    nrow() |>
    expect_gt(1)
  expect_equal(x, y)
})

test_that("`slice_head()` works with `count(country)` for GBIF", {
  skip_galah()
  galah_config(atlas = "GBIF")
  x_default <- request_data() |>
    count(country) |>
    collect() |>
    skip_unavailable()
  x_5 <- request_data() |>
    count(country) |>
    slice_head(n = 5) |>
    collect() |>
    skip_unavailable()
  expect_gte(nrow(x_default), 200)
  expect_equal(nrow(x_5), 5)
})

test_that("`show_values()` fails for unsearchable GBIF fields", {
  skip_galah()
  galah_config(run_checks = TRUE)
  x <- request_metadata(from = "GBIF") |>
    filter(field == "accessRights") |>
    unnest() |>
    collapse() |>
    expect_error()
  galah_config(run_checks = FALSE)
})

test_that("`show_all(collections)` works for GBIF", {
  skip_galah()
  x <- show_all(collections, limit = 10) |>
    skip_unavailable()
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  ## FIXME: not coded slice_head() yet
  # y <- request_metadata(type = "collections") |>
  #   slice_head(n = 10) |>
  #   collect()
  # expect_equal(x, y)
  z <- request_metadata() |>
    filter(collection == "australia") |>
    collect() |>
    skip_unavailable()
  expect_equal(nrow(z), 20)
})

test_that("`show_all(datasets)` works for GBIF", {
  skip_galah()
  x <- show_all(datasets, limit = 10) |>
    skip_unavailable()
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  z <- request_metadata() |>
    filter(dataset == "avian") |>
    collect() |>
    skip_unavailable()
  expect_gte(nrow(z), 10) # historically this was n = 20
})

test_that("`show_all(providers)` works for GBIF", {
  skip_galah()
  x <- show_all(providers, limit = 10) |>
    skip_unavailable()
  expect_equal(nrow(x), 10)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  z <- request_metadata() |>
    filter(provider == "herbarium") |>
    collect() |>
    skip_unavailable()
  expect_equal(nrow(z), 20)
})

test_that("`show_all(reasons)` fails for GBIF", {
  expect_error(show_all(reasons))
})

test_that("`show_all(assertions)` works for GBIF", {
  x <- show_all(assertions) |>
    skip_unavailable()
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
  skip_galah()
  x <- search_taxa("Mammalia") |>
    skip_unavailable()
  expect_equal(nrow(x), 1)
  expect_gte(ncol(x), 1)
  expect_true(colnames(x)[1] == "search_term")
  expect_true(x$class == "Mammalia")
})

test_that("`search_all(taxa)` works using a tibble for GBIF", {
  skip_galah()
  x <- search_all(taxa, 
                  data.frame(kingdom = "Animalia", 
                             phylum = "Chordata")) |>
    skip_unavailable()
  expect_equal(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`unnest()` fails for taxonomic queries to GBIF", {
  # NOTE: this *should* be temporary, until we can get CoL working.
  # e.g. see rcol::clb_children() for an example
  request_metadata() |>
    identify("Crinia") |>
    unnest() |>
    capture() |>
    expect_error()
})

test_that("`search_all(taxa)` works using a muti-row tibble for GBIF", {
  skip_galah()
  x <- search_all(taxa, 
                  data.frame(kingdom = "Animalia", 
                             phylum = "Chordata",
                             class = c("Aves", "Amphibia"))) |>
    skip_unavailable()
  expect_equal(nrow(x), 2)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  # check that no taxon concept IDs consist solely of numbers (i.e. no letters)
  # Since update to CoL that should be impossible
  grepl("^[[:digit:]]+$", x$taxon_concept_id) |>
    any() |>
    expect_false()
})

test_that("`search_all(identifiers)` works for GBIF", {
  skip_galah()
  x <- search_all(identifiers, "359") |>
    skip_unavailable()
  expect_equal(nrow(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

galah_config(verbose = TRUE)

test_that("`search_all(datasets)` works for GBIF", {
  skip_galah()
  x <- search_all(datasets, "Mammals") |>
    skip_unavailable()
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`search_all(collections)` works for GBIF", {
  skip_galah()
  x <- search_all(collections, "Museum") |>
    skip_unavailable()
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`search_all(providers)` works for GBIF", {
  skip_galah()
  x <- search_all(providers, "Frog") |>
    skip_unavailable()
  expect_lte(nrow(x), 20)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

galah_config(verbose = FALSE)

## count() tests

test_that("`count()` works for GBIF", {
  skip_galah()
  galah_call() |>
    count() |>
    collect() |>
    dplyr::pull("count") |>
    expect_gt(0)
})

test_that("`count()` errors when real but non-indexed fields are requested", {
  skip_galah()

  galah_config(run_checks = TRUE)

  # invalid fields
  galah_call() |>
    filter(something == 9) |>
    count() |>
    collapse() |>
    expect_error()

  # real, but not indexed, group_by statement
  request_data() |>
    filter(class == "Mammalia") |>
    group_by(order) |>
    count() |>
    collapse() |>
    expect_error()

   galah_config(run_checks = FALSE)
})

test_that("`count()` works with `identify()` for GBIF", {
  skip_galah()
  x <- request_data() |>
    identify("Psittaciformes") |>
    group_by(classKey) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 2)
  stringr::str_detect(x$classKey, "^[:digit:]+$") |>
    expect_false() # not using old (numeric) backbone
})

test_that("`count()` works with `slice_head()` for GBIF", {
  skip_galah()
  # collapse
  x <- request_data() |>
    count(country) |>
    slice_head(n = 5) |>
    collect() |>
    skip_unavailable()
  expect_equal(nrow(x), 5)
  expect_equal(ncol(x), 2)
})

galah_config(run_checks = TRUE)

test_that("`count()` works with 2 `group_by` args for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x <- galah_call() |>
    filter(year >= 2020) |>
    group_by(year, basisOfRecord) |>
    count() |>
    collapse() |>
    skip_unavailable()
  expect_s3_class(x, "query")
  # expect_equal(length(x), 6) # for some reason this fails on Positron?!?!?!!
  expect_contains(names(x), 
               c("type", "url", "headers", "request")) # "options", "body" ?
  expect_equal(x$type, "data/occurrences-count-groupby")
  # compute
  y <- compute(x) |>
    skip_unavailable()
  expect_s3_class(y, "computed_query")
  # collect
  Sys.sleep(pause_time * 5)
  z <- collect(y) |>
    skip_unavailable()
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

test_that("`count()` works with `identify` for GBIF when `run_checks` = TRUE", {
  skip_galah()
  Sys.sleep(pause_time)
  galah_config(run_checks = TRUE)
  # collapse
  x <- request_data() |>
    identify("Litoria peronii") |>
    filter(year >= 2020, 
           basisOfRecord == "HUMAN_OBSERVATION") |>
    count() |>
    collapse() |>
    skip_unavailable()
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
  Sys.sleep(pause_time * 5)
  z <- collect(y, wait = FALSE) |>
    skip_unavailable(other_classes = "computed_query")
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gt(z$count, 1)
  expect_equal(nrow(z), 1)
})

test_that("`count()` works with `geolocate_polygon()` for GBIF", {
  skip_galah()
  # errors when points given clockwise
  # FIXME: This has been disabled at some point 
  # `st_sfc()` has a `check_ring_dir` argument that might help
  # wkt <- "POLYGON((142.36 -29.01,142.74 -29.01,142.74 -29.39,142.36 -29.39,142.36 -29.01))"
  # expect_error({galah_call() |>
  #    galah_polygon(wkt) |>
  #    count() |>
  #    collect()})
  # works when points given counter-clockwise
  wkt <- "POLYGON((142.36 -29.01,142.36 -29.39,142.74 -29.39,142.74 -29.01,142.36 -29.01))"
  result <- galah_call() |>
    filter(year == 2022) |>
    geolocate_polygon(wkt) |>
    count() |>
    collect() |>
    skip_unavailable()
  # compare against a temporal query in the same place
  result_taxa <- galah_call() |>
    filter(year == 2022) |>
    count() |>
    collect() |>
    skip_unavailable()
  # compare against a purely spatial query
  result_space <- galah_call() |>
    geolocate_polygon(wkt) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lt(result$count, result_taxa$count)
  expect_lt(result$count, result_space$count)
})

test_that("`count()` works with `geolocate_radius()` for GBIF", {
  skip_galah()
  # ditto for a point and radius
  result <- galah_call() |>
    filter(year == 2022) |>
    geolocate_radius(lat = -33.7,
                     lon = 151.3,
                     radius = 5) |>
    count() |>
    collect() |>
    skip_unavailable()
  result_space <- galah_call() |>
    geolocate_radius(lat = -33.7,
                     lon = 151.3,
                     radius = 5) |>
    count() |>
    collect() |>
    skip_unavailable()
  result_taxa <- galah_call() |>
    filter(year == 2022) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lt(result$count, result_taxa$count)
  expect_lt(result$count, result_space$count)
})

test_that("`count()` queries work with `authenticate()` for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x_compute <- request_data(from = "GBIF") |>
    authenticate(username = "atlasoflivingaustralia",
                 email = "ala4r@ala.org.au",
                 password = "galah-gbif-test-login") |>
    filter(year == 2010) |>
    count() |>
    compute() |>
    skip_unavailable()
  Sys.sleep(pause_time * 5)
  x <- collect(x_compute, wait = FALSE) |>
    skip_unavailable(other_classes = "computed_query")
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`atlas_counts()` fails for GBIF when `type = 'species'`", {
  expect_error(atlas_counts(type = "species"))
})


## glimpse() tests

test_that("`glimpse()` works for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  galah_config(atlas = "GBIF",
              username = "atlasoflivingaustralia",
              email = "ala4r@ala.org.au",
              password = "galah-gbif-test-login")
  x <- request_data(from = "GBIF") |>
    filter(year == 2025) |>
    glimpse() |>
    collect() |>
    skip_unavailable()
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
  skip_galah()
  Sys.sleep(pause_time)
  galah_config(atlas = "GBIF",
              username = "atlasoflivingaustralia",
              email = "ala4r@ala.org.au",
              password = "galah-gbif-test-login")
  x <- request_data(from = "GBIF") |>
    identify("Eolophus roseicapilla") |>
    filter(year == 2010) |>
    glimpse() |>
    collect() |>
    skip_unavailable()
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

## occurrence tests

test_that("`atlas_occurrences()` works for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x <- galah_call(from = "GBIF") |>
    filter(year == 1800,
           classKey == "V2",
           country == "AU") |>
    atlas_occurrences() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(x), 10)
  expect_gt(ncol(x), 10)
})

test_that("`atlas_occurrences()` works with `select()` for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x <- galah_call(from = "GBIF") |>
    filter(year == 1800,
           classKey == "V2",
           country == "AU") |>
    select(countryCode, species, eventDate) |>
    atlas_occurrences() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(x), 10)
  expect_equal(ncol(x), 3)
})

test_that("invalid fields are caught from `select()` for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x <- galah_call(from = "GBIF") |>
    filter(year == 1800,
           classKey == "V2",
           country == "AU") |> # this uses `check_fields_gbif_predicates()`
    select(species, eventDate, aRandomTextString) |> # this uses `check_fields_gbif_counts()`
    atlas_occurrences() |>  
    expect_error()
})

test_that("`atlas_species()` works for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x <- request_data(type = "species",
                    from = "GBIF") |>
    authenticate(username = "atlasoflivingaustralia",
                 email = "ala4r@ala.org.au",
                 password = "galah-gbif-test-login") |>
    filter(year == 2010) |>
    identify("Crinia") |>
    collapse() |>
    skip_unavailable()
  expect_s3_class(x, "query")
  expect_equal(length(x), 7)
  expect_equal(names(x), 
               c("type", "atlas", "url", "headers", "options", "body", "request"))
  expect_equal(x$type, "data/species")
  y <- compute(x) |>
    skip_unavailable()
  expect_s3_class(y, "computed_query")
  Sys.sleep(pause_time * 5)
  z <- collect(y, wait = FALSE) |>
    skip_unavailable(other_classes = "computed_query")
  expect_gt(nrow(z), 10) # n = 14
  expect_gt(ncol(z), 5)  # n = 22
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
})

test_that("`distinct()` queries accept `select()` for GBIF", {
  skip_galah()
  Sys.sleep(pause_time)
  x_compute <- request_data(from = "GBIF") |>
    authenticate(username = "atlasoflivingaustralia",
                 email = "ala4r@ala.org.au",
                 password = "galah-gbif-test-login") |>
    filter(year == 2010) |>
    distinct(species_guid, .keep_all = TRUE) |>
    identify("Crinia") |>
    select(scientificName, speciesKey) |>
    compute() |>
    skip_unavailable()
  Sys.sleep(pause_time * 5)
  x <- collect(x_compute, wait = FALSE) |>
    skip_unavailable(other_classes = "computed_query")
  expect_gt(nrow(x), 10) # 
  expect_equal(ncol(x), 2)
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
})

test_that("`atlas_media()` fails for GBIF", {
  skip_galah()
  galah_call() |>
    filter(year == 2022) |>
    atlas_media() |>
    expect_error()
})

test_that("`collect()` works for GBIF with `type = 'occurrences' or 'occurrences-doi'` ", {
  skip_galah()
  Sys.sleep(pause_time)
  # collapse
  base_query <- request_data() |>
    identify("Vulpes vulpes") |>
    filter(year == 1900)
  count <- base_query |> 
    count() |> 
    collect() |>
    skip_unavailable()
  Sys.sleep(pause_time)
  x <- base_query |>
    collapse() |>
    skip_unavailable()
  # NOTE: the above query should return 72 records (tested 2025-06-10)
  expect_s3_class(x, "query")
  expect_equal(names(x), 
               c("type", "atlas", "url", "headers", "options", "body", "request"))
  expect_equal(x$type, "data/occurrences")
  # compute
  y <- compute(x) |>
    skip_unavailable()
  expect_true(inherits(y, "computed_query"))
  expect_true(y$type == "data/occurrences")  
  expect_true(any(names(y) == "status"))
  # collect
  Sys.sleep(pause_time)
  z <- collect(y, wait = TRUE) |>
    skip_unavailable()
  expect_gt(nrow(z), 0)
  expect_gt(ncol(z), 0)
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
  # expect_equal(nrow(z), count$count) # differs, for some reason
  expect_true(!is.null(attributes(z)$doi))

  # FIXME: need DOI search test
  Sys.sleep(pause_time)
  recent_doi <- attributes(z)$doi
  a <- galah_call() |>
   filter(doi == recent_doi) |>
   collect() |>
    skip_unavailable()
  expect_equal(a, z, ignore_attr = TRUE)
  expect_equal(attributes(a)$doi, attributes(z)$doi)
})

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config, pause_time)
