context("Test ALA data profiles")

test_that("ALA data profiles behaves as expected", {
  skip_on_cran()
  expect_equal(class(find_profiles()), "data.frame")
  expect_equal(ncol(find_profiles()), 4)
})

test_that("find_profile_attributes checks input", {
  skip_on_cran()
  expect_error(find_profile_attributes(10))
  expect_error(find_profile_attributes("bad_profile"))
})

test_that("find_profile_attributes returns dataframe", {
  skip_on_cran()
  expect_equal(ncol(find_profile_attributes(2)), 2)
  expect_equal(ncol(find_profile_attributes("Data licensed for all uses")), 2)
  expect_true(is(find_profile_attributes("ALA"), "data.frame"))
})
