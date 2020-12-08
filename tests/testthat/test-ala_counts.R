context("Test ala counts")

test_that("ala counts checks inputs", {
  skip_on_cran()
  # ALA counts with no arguments gives the total number of records in the ALA
  expect_gt(ala_counts(), 90000000)
  # invalid facet
  expect_error(ala_counts(breakdown = "bad_facet"))
  
  # invalid filter
  expect_error(ala_counts(filters = ala_filters(list(bad_facet = 'test'))))
  
  # too many filters
  filters <- ala_filters(sapply(ala_fields("assertion")$name,
                                function(x){ return(TRUE) }))
  expect_error(ala_counts(filters))
})

test_that("ala counts returns expected outputs", {
  skip_on_cran()
  expect_type(ala_counts(
    taxon_id = "https://id.biodiversity.org.au/taxon/apni/51302291"),
    "integer")
  expect_equal(class(ala_counts(
    taxon_id = "https://id.biodiversity.org.au/taxon/apni/51302291",
                                breakdown = "basis_of_record")), "data.frame")
  
  expect_warning(ala_counts(breakdown = 'phylum'))
})

test_that("ala counts works with filters", {
  skip_on_cran()
  expect_lt(ala_counts(filters = ala_filters(list(year = 2000))),
            ala_counts())
})

test_that("ala_counts handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  wkt <- readLines('../testdata/short_act_wkt.txt')
  expect_lt(ala_counts(geometry = ala_geometry(wkt)), ala_counts())
})

test_that("ala counts handles queries with no records", {
  skip_on_cran()
  filters <- ala_filters(list(kingdom = 'non-existent'))
  expect_s3_class(ala_counts(filters = filters,
                             breakdown = 'basis_of_record'), "data.frame")
})

test_that("ala_counts works with long queries", {
  skip_on_cran()
  taxa <- ala_taxa("Hymenoptera", return_children = TRUE)
  filters <- ala_filters(data_quality_profile = "ALA")
  expect_gt(ala_counts(taxa, filters), 0)
})

test_that("ala occurrences handles long queries with pagination", {
  skip_on_cran()
  taxa <- ala_taxa("Hymenoptera", return_children = TRUE)
  filters <- ala_filters(data_quality_profile = "ALA")
  expect_equal(nrow(ala_counts(breakdown = "eventDate", limit = 101)), 101)
})
