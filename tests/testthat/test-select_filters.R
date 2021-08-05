context("Test ALA filters")

test_that("select_filters builds data quality filters", {
  # vcr can't handle this request
  skip_on_cran()
  filters <- select_filters(profile = "ALA")
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 0)
  expect_equal(names(filters), c("variable", "logical", "value", "query"))
  expect_equal(attr(filters, "dq_profile"), "ALA")
})

vcr::use_cassette("select_filter_invalid_profile", {
  test_that("select_filters gives an error for invalid profile", {
    expect_error(select_filters(profile = "bad"))
  })
})

vcr::use_cassette("select_filter_assertion", {
  test_that("select_filters handles assertion filters", {
    filters <- select_filters(ZERO_COORDINATE = FALSE)
    expect_s3_class(filters, "data.frame")
    expect_true("assertions" %in% filters$variable)
    # should convert logical to string
    expect_equal(unlist(filters$value), "ZERO_COORDINATE")
  })
})

vcr::use_cassette("select_filters_invalid", {
  test_that("select_filters validates filters", {
    expect_error(select_filters(invalid_filter = 'value'))
  })
})

test_that("select_filters skips checks if requested", {
  galah_config(run_checks = FALSE)
  expect_silent(select_filters(random = "filter"))
  galah_config(run_checks = TRUE)
})

test_that("exclude negates filters", {
  expect_warning(expect_s3_class(exclude(2021), c("exclude", "numeric")))
})

test_that("select_filters handles logical statements", {
  skip_on_cran()
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

