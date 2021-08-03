context("Test ala counts")

test_that("ala_counts checks group_by field", {
  expect_error(ala_counts(group_by = "invalid"))
})

vcr::use_cassette("ala_count", {
  test_that("ala_counts works with no arguments", {
    # ALA counts with no arguments gives the total number of records in the ALA
    count <- ala_counts()
    expect_gt(count, 0)
  })
})

vcr::use_cassette("taxa_count", {
  test_that("ala_counts returns expected output", {
    counts <- ala_counts(taxa = select_taxa("Mammalia"))
    expect_type(counts, "integer")
  })
})

vcr::use_cassette("record_count_group_by", {
  test_that("grouped ala_counts returns expected output", {
    counts <- ala_counts(taxa = select_taxa("Mammalia"),
                         group_by = "basisOfRecord")
    expect_s3_class(counts, "data.frame")
    expect_equal(names(counts), c("basisOfRecord", "count"))
  })
})

vcr::use_cassette("record_count_no_limit", {
  test_that("ala counts returns all counts if no limit is provided", {
    counts <- ala_counts(group_by = "month", limit = NULL)
    expect_s3_class(counts, "data.frame")
    expect_equal(nrow(counts), 12)
  })
})

vcr::use_cassette("species_count_group_by", {
  test_that("grouped ala_counts for species returns expected output", {
    counts <- ala_counts(taxa = select_taxa("Mammalia"),
                         filters = select_filters(year = 2020),
                         group_by = "month",
                         type = "species")
    expect_s3_class(counts, "data.frame")
    expect_equal(names(counts), c("month", "count"))
  })
})

vcr::use_cassette("species_count", {
  test_that("ala_counts returns species counts", {
    counts <- ala_counts(type = "species")
    expect_type(counts, "integer")
    expect_gt(counts, 0)
  })
})

vcr::use_cassette("empty_count", {
  test_that("ala_counts handles empty count", {
    filters <- select_filters(kingdom = 'non-existent')
    counts <- ala_counts(filters = filters, group_by = "basisOfRecord")
    expect_s3_class(counts, "data.frame")
    expect_equal(nrow(counts), 0)
    expect_equal(names(counts), c("name", "count"))
  })
})

vcr::use_cassette("paginated_counts", {
  test_that("ala_counts handles pagination", {
    counts <- ala_counts(group_by = "year", limit = 101)
    expect_s3_class(counts, "data.frame")
    expect_equal(nrow(counts), 101)
    expect_equal(names(counts), c("year", "count"))
  })
})

test_that("ala_counts caches as expected", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  filters <- select_filters(basisOfRecord = "FossilSpecimen")
  counts <- ala_counts(filters = filters, group_by = "year", limit = 100)
  expect_message(counts2 <- ala_counts(filters = filters, group_by = "year",
                                       limit = 100),
                 "Using cached file")
  expect_equal(nrow(counts), nrow(counts2))
})

test_that("ala_counts returns consistent data from cached/non-cached calls", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  counts1 <- ala_counts(group = "year")
  counts2 <- ala_counts(group = "year")
  expect_equal(class(counts1$year),
               class(counts2$year))
})
