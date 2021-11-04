context("Test ALA filters")

test_that("select_filters builds data quality filters", {
  # vcr can't handle this request
  # skip_on_cran()
  galah_config(run_checks = FALSE)
  filters <- select_filters(profile = "ALA")
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 0)
  expect_equal(names(filters), c("variable", "logical", "value", "query"))
  expect_equal(attr(filters, "dq_profile"), "ALA")
  galah_config(run_checks = TRUE)
})


test_that("select_filters gives an error for invalid profile", {
  vcr::use_cassette("select_filter_invalid_profile", {
    expect_error(select_filters(profile = "bad"))
  })
})

vcr::use_cassette("select_filter_assertion", {
  test_that("select_filters handles assertion filters", {
    filters <- select_filters(ZERO_COORDINATE = FALSE)
    expect_s3_class(filters, "data.frame")
    expect_true(grepl("assertions", filters$query))
    
    # negative assertions:
    # select_filters(BASIS_OF_RECORD_INVALID = FALSE)
  })
})

vcr::use_cassette("select_filters_invalid", {
  test_that("select_filters validates filters", {
    galah_config(run_checks = TRUE)
    expect_message(select_filters(invalid_filter = 'value'))
  })
})

test_that("select_filters skips checks if requested", {
  galah_config(run_checks = FALSE)
  expect_silent(select_filters(random = "filter"))
  galah_config(run_checks = TRUE)
})

test_that("select_filters handles basic queries", {
  galah_config(run_checks = FALSE)
  
  # empty queries
  filters <- select_filters()
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 0)
   
  # a single value of a single field
  filters <- select_filters(year = 2010)
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 1)
  
  # a single value from each of two fields
  filters <- select_filters(year = 2010, basisOfRecord = "HUMAN_OBSERVATION")
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 2)

  galah_config(run_checks = TRUE)
})

test_that("select_filters handles logical statements", {
  # skip_on_cran()
  galah_config(run_checks = FALSE)
  expect_equal(select_filters(year >= 2010 & year < 2020),
               select_filters(year >= 2010, year < 2020))
  filters <- select_filters(cl22 >= "Tasmania")
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  
  filters <- select_filters(year == 2010 | year == 2021)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  
  filters <- select_filters(year >= 2010 & year != 2021)
  expect_equal(nrow(filters), 2)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  
  filters <- select_filters(
    basisOfRecord = "HumanObservation",
    year >= 2010,
    stateProvince = "New South Wales")
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters),3)
})

test_that("select_filters handles objects and functions", {
  # test for a vector
  filters <- select_filters(year = c(2010, 2021))
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)

  # using an object as a field
  field <- "year"
  filters <- select_filters(field = 2010)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("year", filters$query))
  
  # using an object as a value
  value <- "2010"
  filters <- select_filters(year = value)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)
  expect_match(filters$query, "(year:\"2010\")")
  
  # using an object with length >1 as a value
  years <- c(2010, 2021) 
  filters <- select_filters(year = years)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
  
  ## using an object as an equation
  ## NOTE: This fails because `input_text` is not found. Appears to be an issue with testthat
  # input_text <- "year = 2010"
  # filters <- select_filters(input_text)
  # expect_s3_class(filters, c("data.frame", "ala_filters"))
  # expect_equal(nrow(filters), 1)
  # expect_true(grepl("2010", filters$query))
  
  # using a function to define an equation
  filters <- select_filters(paste("year", "2010", sep = " = "))
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
  
  # # using a pre-defined function to define an equation
  ## NOTE: This fails because `input_paste` is not found. Appears to be an issue with testthat
  # input_paste <- paste("year", "2010", sep = " = ")
  # filters <- select_filters(input_paste)
  # expect_s3_class(filters, c("data.frame", "ala_filters"))
  # expect_equal(nrow(filters), 1)
  # expect_true(grepl("2010", filters$query))

  # quoting an equation that contains objects
  field <- "year"
  value <- "2010"
  filters <- select_filters("field = value")
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
  
})

## TODO: add vcr to this
test_that("select_filters handles taxonomic queries", {
  # ensure a taxonomic query to select_filters works
  filters <- select_filters(taxonConceptID = select_taxa("Animalia")$taxon_concept_id)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 1)
  expect_false(grepl("select_taxa", filters$query))

  # as above, but with a negation
  filters <- select_filters(
    taxonConceptID = select_taxa("Animalia")$taxon_concept_id,
    taxonConceptID != select_taxa("Chordata")$taxon_concept_id)
  expect_s3_class(filters, c("data.frame", "ala_filters"))
  expect_equal(nrow(filters), 2)
  expect_false(any(grepl("select_taxa", filters$query)))
})