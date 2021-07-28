context("Test ALA filters")

test_that("select_filters builds data quality filters", {
  # vcr can't handle this request
  skip_on_cran()
  filters <- select_filters(profile = "ALA")
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 1)
  expect_equal(names(filters), c("name", "include", "value"))
  expect_equal(filters$name, "profile")
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
    expect_true("assertions" %in% filters$name)
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
  ala_config(run_checks = FALSE)
  expect_silent(select_filters(random = "filter"))
  ala_config(run_checks = TRUE)
})