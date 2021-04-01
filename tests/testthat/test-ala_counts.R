context("Test ala counts")

test_that("ala counts checks inputs", {
  skip_on_cran()
  # ALA counts with no arguments gives the total number of records in the ALA
  expect_gt(ala_counts(), 90000000)
  # invalid facet
  expect_error(ala_counts(group_by = "bad_facet"))
  
  # invalid filter
  expect_error(ala_counts(filters = select_filters(bad_facet = 'test')))
  
  # too many filters
  filters <- select_filters(sapply(search_fields(type = "assertions")$id,
                                function(x){ return(TRUE) }))
  expect_error(ala_counts(filters))
})

test_that("ala counts returns expected outputs", {
  skip_on_cran()
  expect_type(ala_counts(
    taxa = "https://id.biodiversity.org.au/taxon/apni/51302291"),
    "integer")
  expect_equal(class(ala_counts(
    taxa = "https://id.biodiversity.org.au/taxon/apni/51302291",
                                group_by = "basis_of_record")), "data.frame")
  
})

test_that("ala_counts works with filters", {
  skip_on_cran()
  expect_lt(ala_counts(filters = select_filters(year = 2000)),
            ala_counts())
})

test_that("ala_counts returns species counts", {
  skip_on_cran()
  expect_gt(ala_counts(type = "species"), 140000)
})

test_that("ala_counts handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  wkt <- readLines('../testdata/short_act_wkt.txt')
  expect_lt(ala_counts(locations = select_locations(wkt = wkt)), ala_counts())
})

test_that("ala counts handles queries with no records", {
  skip_on_cran()
  filters <- select_filters(kingdom = 'non-existent')
  expect_s3_class(ala_counts(filters = filters,
                             group_by = 'basis_of_record'), "data.frame")
})

test_that("ala_counts works with long queries", {
  skip_on_cran()
  taxa <- select_taxa("Hymenoptera", children = TRUE)
  filters <- select_filters(profile = "ALA")
  expect_gt(ala_counts(taxa, filters), 0)
})

test_that("ala occurrences handles long queries with pagination", {
  skip_on_cran()
  expect_equal(nrow(ala_counts(group_by = "year", limit = 101)), 101)
})
