context("Test ala counts")

test_that("ala_counts checks group_by field", {
  expect_error(ala_counts(group_by = "invalid"))
})


test_that("ala_counts works with no arguments", {
  vcr::use_cassette("ala_count", {
    count <- ala_counts()
  })
  # ALA counts with no arguments gives the total number of records in the ALA
  expect_gt(count, 0)
})

test_that("ala_counts returns expected output", {
  vcr::use_cassette("taxa_count", {
    counts <- ala_counts(taxa = select_taxa("Mammalia"))
  })
  expect_type(counts, "integer")
})


test_that("grouped ala_counts returns expected output", {
  vcr::use_cassette("record_count_group_by", {
    counts <- ala_counts(
      taxa = select_taxa("Mammalia"),
      group_by = "basisOfRecord"
    )
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(names(counts), c("basisOfRecord", "count"))
})


test_that("ala counts returns all counts if no limit is provided", {
  vcr::use_cassette("record_count_no_limit", {
    counts <- ala_counts(group_by = "month", limit = NULL)
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(nrow(counts), 12)
})


test_that("grouped ala_counts for species returns expected output", {
  vcr::use_cassette("species_count_group_by", {
    counts <- ala_counts(
      taxa = select_taxa("Mammalia"),
      filters = galah_filter(year = 2020),
      group_by = "month",
      type = "species"
    )
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(names(counts), c("month", "count"))
})


test_that("ala_counts returns species counts", {
  vcr::use_cassette("species_count", {
    counts <- ala_counts(type = "species")
  })
  expect_type(counts, "integer")
  expect_gt(counts, 0)
})


test_that("ala_counts handles empty count", {
  vcr::use_cassette("empty_count", {
    filters <- galah_filter(kingdom = "non-existent")
    counts <- ala_counts(filters = filters, group_by = "basisOfRecord")
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(nrow(counts), 0)
  expect_equal(names(counts), c("name", "count"))
})


test_that("ala_counts handles pagination", {
  vcr::use_cassette("paginated_counts", {
    counts <- ala_counts(group_by = "year", limit = 101)
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(nrow(counts), 101)
  expect_equal(names(counts), c("year", "count"))
})

test_that("ala_counts caches as expected", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  filters <- galah_filter(basisOfRecord = "FossilSpecimen")
  counts <- ala_counts(filters = filters, group_by = "year", limit = 100)
  expect_message(
    counts2 <- ala_counts(
      filters = filters, group_by = "year",
      limit = 100
    ),
    "Using cached file"
  )
  expect_equal(nrow(counts), nrow(counts2))
})

test_that("ala_counts returns consistent data from cached/non-cached calls", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  counts1 <- ala_counts(group = "year")
  counts2 <- ala_counts(group = "year")
  expect_equal(
    class(counts1$year),
    class(counts2$year)
  )
})
