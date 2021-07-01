context("Test ala counts")

test_that("ala counts checks inputs", {
  skip_on_cran()
  # ALA counts with no arguments gives the total number of records in the ALA
  expect_gt(ala_counts(), 0)
  # invalid facet
  expect_error(ala_counts(group_by = "bad_facet"))
  
  # invalid filter
  expect_error(ala_counts(filters = select_filters(bad_facet = 'test')))
  
})

test_that("ala counts returns expected outputs", {
  skip_on_cran()
  expect_type(ala_counts(
    taxa = "https://id.biodiversity.org.au/taxon/apni/51302291"),
    "integer")
  expect_equal(class(ala_counts(
    taxa = "https://id.biodiversity.org.au/taxon/apni/51302291",
                                group_by = "basisOfRecord")), "data.frame")
  
})

test_that("ala_counts works with filters", {
  skip_on_cran()
  expect_lt(ala_counts(filters = select_filters(year = 2000)),
            ala_counts())
})

test_that("ala_counts returns species counts", {
  skip_on_cran()
  expect_gt(ala_counts(type = "species"), 0)
})

test_that("ala_counts handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  wkt <- readLines('../testdata/short_act_wkt.txt')
  expect_lt(ala_counts(locations = select_locations(wkt)), ala_counts())
})

test_that("ala counts handles queries with no records", {
  skip_on_cran()
  filters <- select_filters(kingdom = 'non-existent')
  expect_s3_class(ala_counts(filters = filters,
                             group_by = 'basisOfRecord'), "data.frame")
})

test_that("ala_counts works with long queries", {
  skip_on_cran()
  taxa <- select_taxa("Hymenoptera", children = TRUE)
  filters <- select_filters(profile = "ALA")
  expect_gt(ala_counts(taxa, filters), 0)
})

test_that("ala_counts works with combinations of filters", {
  skip_on_cran()
  filters <- select_filters(profile = "ALA", year = 2015, basisOfRecord = "PreservedSpecimen")
  expect_gt(ala_counts(filters = filters), 0)
})

test_that("ala_counts handles pagination", {
  skip_on_cran()
  expect_equal(nrow(ala_counts(group_by = "year", limit = 101)), 101)
})

test_that("ala_counts handles multi-filter queries with pagination", {
  skip_on_cran()
  expect_equal(
    nrow(ala_counts(taxa = select_taxa("Anas anas"),
                    filters = select_filters(basisOfRecord = "HumanObservation"),
                    group_by = "year", limit = 101)),
    101)
})

test_that("ala_counts works with assertions", {
  skip_on_cran()
  expect_equal(
    ala_counts(),
    ala_counts(filters = select_filters(CONTINENT_INVALID = FALSE)) +
      ala_counts(filters = select_filters(CONTINENT_INVALID = TRUE)))
})

test_that("ala_counts caches as expected", {
  skip_on_cran()
  ala_config(caching = TRUE, verbose = TRUE)
  filters <- select_filters(basisOfRecord = "FossilSpecimen")
  counts <- ala_counts(group_by = "year", limit = 100)
  expect_message(counts2 <- ala_counts(group_by = "year", limit = 100),
                 "Using cached file")
  expect_equal(nrow(counts), nrow(counts2))
})


test_that("ala_counts returns consistent data from cached/non-cached calls", {
  skip_on_cran()
  ala_config(caching = TRUE, verbose = TRUE)
  counts1 <- ala_counts(group = "year")
  counts2 <- ala_counts(group = "year")
  expect_equal(class(counts1$year),
               class(counts2$year))
})
