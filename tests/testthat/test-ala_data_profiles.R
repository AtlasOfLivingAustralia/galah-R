context("Test ALA data profiles")

test_that("ALA data profiles behaves as expected", {
  expect_equal(class(ala_data_profiles()), "data.frame")
  expect_equal(ncol(ala_data_profiles()), 4)
})

test_that("ala_quality_filters checks input", {
  expect_error(ala_quality_filters(10))
  expect_error(ala_quality_filters("bad_profile"))
})

test_that("ala_quality_filters returns dataframe", {
  expect_equal(ncol(ala_quality_filters(2)), 2)
  expect_equal(ncol(ala_quality_filters("Data licensed for all uses")), 2)
  expect_true(is(ala_quality_filters("ALA"), "data.frame"))
})
