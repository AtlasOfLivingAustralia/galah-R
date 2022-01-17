context("Test ALA data profiles")
# NOTE: vcr not used here as it can't handle dq service

 
test_that("show_all_profiles returns profiles", {
  skip_on_cran()
  profiles <- show_all_profiles()
  expect_s3_class(profiles, "data.frame")
  expect_equal(ncol(profiles), 4)
})

test_that("search_profile_attributes checks input", {
  expect_error(search_profile_attributes(10))
  expect_error(search_profile_attributes("invalid"))
})

test_that("search_profile_attributes returns dataframe", {
  skip_on_cran()
  atts <- search_profile_attributes(92)
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, "data.frame")
  atts <- search_profile_attributes("ALA")
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, "data.frame")
  atts <- search_profile_attributes("ALA General")
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, "data.frame")
})
