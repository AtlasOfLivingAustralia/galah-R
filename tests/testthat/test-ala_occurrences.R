context("Get occurrence data")

vcr::use_cassette("ala_occurrences_no_filters", {
  test_that("ala_occurrences doesn't allow large downloads", {
    expect_error(ala_occurrences())
  })
})

test_that("ala_occurrences gives a nice error for invalid emails", {
  skip_on_cran()
  galah_config(email = "test@test.org.au")
  expect_error(ala_occurrences(taxa = select_taxa("Thylacinus cynocephalus")))
  galah_config(email = "test@test.org.au")
})

test_that("ala_occurrences fails nicely if no email is provided", {
  galah_config(email = "", run_checks = FALSE)
  expect_error(ala_occurrences(filters = select_filters(year = 1900)))
  galah_config(email = "ala4r@ala.org.au")
})
  

  # test all filters and type of columns in one call
test_that("ala occurrences returns requested columns", {
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "stateProvince", "ZERO_COORDINATE")
  filters <- select_filters(year = seq(2018, 2020))
  cols <- select_columns(group = "basic", "stateProvince", "ZERO_COORDINATE")
  taxa <- select_taxa("Polytelis swainsonii")
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  locations <- select_locations(poly)
  occ <- ala_occurrences(
    taxa = taxa,
    filters = filters,
    columns = cols,
    locations = locations)
  expect_setequal(names(occ), expected_cols)
  expect_equal(unique(occ$stateProvince), "New South Wales")
})

test_that("ala_occurrences caches data as expected", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  taxa <- select_taxa("Wurmbea dioica")
  filters <- select_filters(year = 2000)
  columns <- select_columns(group = "basic", "basisOfRecord")

  # Download data
  occ <- ala_occurrences(taxa = taxa, filters = filters, columns = columns)
  # Re-download data
  expect_message(
    ala_occurrences(taxa = taxa, filters = filters, columns = columns), 
    "Using cached file")
  galah_config(caching = FALSE)
})

test_that("ala_occurrences downloads data from a DOI", {
  skip_on_cran()
    doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
    occ <- ala_occurrences(doi = doi)
    expect_s3_class(occ, "data.frame")
    expect_gt(nrow(ala_occurrences(doi = doi)), 0)
})

test_that("ala_occurrences checks doi provided", {
  expect_error(ala_occurrences(doi = "random_doi"))
})
