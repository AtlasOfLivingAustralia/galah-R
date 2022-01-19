context("Test atlas_occurrences")

test_that("atlas_occurrences doesn't allow large downloads", {
  expect_error(atlas_occurrences())
})

test_that("atlas_occurrences gives a nice error for invalid emails", {
  skip_on_cran()
  galah_config(email = "test@test.org.au")
  expect_message(atlas_occurrences(identify = galah_identify("Thylacinus cynocephalus")))
  galah_config(email = "test@test.org.au")
})

test_that("atlas_occurrences fails nicely if no email is provided", {
  galah_config(email = "", run_checks = FALSE)
  expect_error(atlas_occurrences(filter = galah_filter(year == 1900)))
  galah_config(email = "ala4r@ala.org.au")
})
  

# test all filters and type of columns in one call
test_that("atlas_occurrences accepts all narrowing functions inline", { 
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "stateProvince", "ZERO_COORDINATE")
  filters <- galah_filter(year >= 2018)
  cols <- galah_select(group = "basic", stateProvince, ZERO_COORDINATE)
  identify <- galah_identify("Polytelis swainsonii")
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  locations <- galah_geolocate(poly)
  occ <- atlas_occurrences(
    identify = identify,
    filter = filters,
    select = cols,
    geolocate = locations)
  expect_setequal(names(occ), expected_cols)
  expect_equal(unique(occ$stateProvince), "New South Wales")
})

# repeat above using galah_call
test_that("atlas_occurrences accepts all narrowing functions in pipe", { 
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "stateProvince", "ZERO_COORDINATE")
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

test_that("atlas_occurrences downloads data from a DOI", {
  skip_on_cran()
    doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
    occ <- atlas_occurrences(doi = doi)
    expect_s3_class(occ, "data.frame")
    expect_gt(nrow(atlas_occurrences(doi = doi)), 0)
})

test_that("atlas_occurrences checks DOI provided", {
  expect_error(atlas_occurrences(doi = "random_doi"))
})
