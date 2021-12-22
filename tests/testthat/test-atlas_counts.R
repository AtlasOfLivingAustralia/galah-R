context("Test atlas_counts")

test_that("atlas_counts checks group_by field", {
  expect_error(atlas_counts(group_by = "invalid"))
})


test_that("atlas_counts works with no arguments", {
  vcr::use_cassette("atlas_count", {
    count <- atlas_counts()
  })
  # atlas_counts with no arguments gives the total number of records in the ALA
  expect_gt(count, 0)
})

test_that("atlas_counts returns expected output", {
  vcr::use_cassette("taxa_count", {
    counts <- atlas_counts(taxa = search_taxa("Mammalia"))
  })
  expect_type(counts$count, "integer")
})


test_that("grouped atlas_counts returns expected output", {
  vcr::use_cassette("record_count_group_by", {
    counts <- atlas_counts(
      taxa = search_taxa("Mammalia"),
      group_by = galah_group_by(basisOfRecord)
    )
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(names(counts), c("basisOfRecord", "count"))
})


test_that("atlas_counts returns all counts if no limit is provided", {
  vcr::use_cassette("record_count_no_limit", {
    counts <- atlas_counts(group_by = galah_group_by(month), 
                           limit = NULL)
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(nrow(counts), 12)
})


test_that("grouped atlas_counts for species returns expected output", {
  vcr::use_cassette("species_count_group_by", {
    counts <- atlas_counts(
      taxa = search_taxa("Mammalia"),
      filter = galah_filter(year == 2020),
      group_by = galah_group_by(month),
      type = "species"
    )
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(names(counts), c("month", "count"))
})


test_that("atlas_counts returns species counts", {
  vcr::use_cassette("species_count", {
    counts <- atlas_counts(type = "species")
  })
  expect_type(counts$count, "integer")
  expect_gt(counts, 0)
})


test_that("atlas_counts handles empty count", {
  vcr::use_cassette("empty_count", {
    filters <- galah_filter(kingdom == "non-existent")
    counts <- atlas_counts(filter = filters, 
                           group_by = galah_group_by(basisOfRecord))
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(nrow(counts), 0)
  expect_equal(names(counts), c("name", "count"))
})


test_that("atlas_counts handles pagination", {
  vcr::use_cassette("paginated_counts", {
    counts <- atlas_counts(group_by = galah_group_by(year), 
                           limit = 101)
  })
  expect_s3_class(counts, "data.frame")
  expect_equal(nrow(counts), 101)
  expect_equal(names(counts), c("year", "count"))
})

test_that("atlas_counts caches as expected", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  filters <- galah_filter(basisOfRecord == "FossilSpecimen")
  counts <- atlas_counts(filter = filters,
                         group_by = galah_group_by(year), 
                         limit = 100)
  expect_message(
    counts2 <- atlas_counts(
      filter = filters, 
      group_by = galah_group_by(year),
      limit = 100
    ),
    "Using cached file"
  )
  expect_equal(nrow(counts), nrow(counts2))
})

test_that("atlas_counts returns consistent data from cached/non-cached calls", {
  skip_on_cran()
  galah_config(caching = TRUE, verbose = TRUE)
  counts1 <- atlas_counts(group_by = galah_group_by(year))
  counts2 <- atlas_counts(group_by = galah_group_by(year))
  expect_equal(
    class(counts1$year),
    class(counts2$year)
  )
})
