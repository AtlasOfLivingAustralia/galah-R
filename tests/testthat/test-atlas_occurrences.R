without_internet({
  test_that("collapse(type = 'occurrences') creates an object, but doesn't ping an API", {
    result <- galah_call() |> 
      identify("Perameles") |>
      collapse()
    expect_equal(length(result), 3)
    expect_true(inherits(result, "query_set"))
    types <- unlist(lapply(result, function(a){a$type}))
    expect_equal(types,
                 c("metadata/reasons",
                   "metadata/taxa-single",
                   "data/occurrences"))
  })
})

test_that("`compute(type = 'occurrences')` works", {
  skip_if_offline()
  base_query <- galah_call() |>
    identify("Vulpes vulpes") |>
    filter(year <= 1900, 
           basisOfRecord == "PRESERVED_SPECIMEN")
  # collapse
  query_collapse <- collapse(base_query)
  expect_true(inherits(result, "query_set"))
  expect_equal(length(result), 3)
  types <- unlist(lapply(query_collapse, function(a){a$type}))
  expect_equal(types,
               c("metadata/fields",
                 "metadata/assertions",
                 "metadata/reasons",
                 "metadata/taxa-single",
                 "data/occurrences"))
  # compute
  response <- compute(base_query)
  expect_true(inherits(response, "query"))
  expect_true(response$type == "data/occurrences")
  expect_equal(names(response),
               c("type",
                 "status",
                 "total_records",
                 "queue_size",
                 "status_url",
                 "cancel_url",
                 "search_url"))  
})


test_that("atlas_occurrences doesn't allow large downloads", {
  galah_config(atlas = "Australia")
  expect_error(atlas_occurrences())
})

test_that("atlas_occurrences gives a nice error for invalid emails", {
  galah_config(email = "test@test.org.au")
  expect_error({
    galah_call() |>
      identify("Perameles") |>
      compute()
  })
  galah_config(email = "ala4r@ala.org.au")
})

without_internet({
  test_that("atlas_occurrences fails nicely if no email is provided", {
    galah_config(email = "", run_checks = FALSE)
    expect_error({
      galah_call() |>
        filter(year == 1900) |>
        compute()
    })
    galah_config(email = "ala4r@ala.org.au")
  })
})

# test all filters and type of columns in one call
test_that("atlas_occurrences accepts all narrowing functions inline", { 
  skip_if_offline()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "occurrenceStatus", "stateProvince", 
                     "ZERO_COORDINATE")
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  occ <- galah_call(type = "occurrences") |>
    identify("Polytelis swainsonii") |>
    filter(year >= 2018) |>
    select(group = "basic", stateProvince, ZERO_COORDINATE) |>
    galah_geolocate(poly) |>
    collect(wait = TRUE)    
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
  expect_setequal(names(occ), expected_cols)
  expect_equal(unique(occ$stateProvince), "New South Wales")
})

# repeat above using `galah_` functions
test_that("atlas_occurrences accepts all narrowing functions in pipe", { 
  skip_if_offline()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "occurrenceStatus", "stateProvince", 
                     "ZERO_COORDINATE")
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  occ <- galah_call() |>
    galah_filter(year >= 2018) |>
    galah_select(group = "basic", stateProvince, ZERO_COORDINATE) |>
    galah_identify("Polytelis swainsonii") |> 
    galah_geolocate(poly) |>
    atlas_occurrences()
  expect_setequal(names(occ), expected_cols)
  expect_equal(unique(occ$stateProvince), "New South Wales")
})

## Caching no longer supported in galah - suggest delete
# test_that("atlas_occurrences caches data as expected", {
#   skip_on_cran()
#   galah_config(caching = TRUE, verbose = TRUE)
#   taxa <- search_taxa("Wurmbea dioica")
#   filter <- galah_filter(year == 2000)
#   columns <- galah_select(group = "basic", basisOfRecord)
# 
#   # Download data
#   occ <- atlas_occurrences(taxa = taxa, filter = filter, select = columns)
#   # Re-download data
#   expect_message(
#     atlas_occurrences(taxa = taxa, filter = filter, select = columns), 
#     "Using cached file")
#   galah_config(caching = FALSE)
# })

test_that("atlas_occurrences does not download data from a DOI", {
  skip_if_offline()
  doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
  expect_error(atlas_occurrences(doi = doi))
})