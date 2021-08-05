context("Test ALA data profiles")

test_that("find_profiles returns profiles", {
  # vcr can't handle dq service
  skip_on_cran()
  profiles <- find_profiles()
  expect_s3_class(profiles, "data.frame")
  expect_equal(ncol(profiles), 4)
})

vcr::use_cassette("find_profiles_attrs_invalid", {
  test_that("find_profile_attributes checks input", {
    expect_error(find_profile_attributes(10))
    expect_error(find_profile_attributes("invalid"))
  })
})

test_that("find_profile_attributes returns dataframe", {
  # vcr can't handle dq service
  skip_on_cran()
  atts <- find_profile_attributes(92)
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, "data.frame")
  atts <- find_profile_attributes("ALA")
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, "data.frame")
  atts <- find_profile_attributes("ALA General")
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, "data.frame")
})
