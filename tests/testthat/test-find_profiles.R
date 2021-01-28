context("Test ALA data profiles")

test_that("ALA data profiles behaves as expected", {
  expect_equal(class(find_profiles()), "data.frame")
  expect_equal(ncol(find_profiles()), 4)
})

test_that("find_profile_filters checks input", {
  expect_error(find_profile_filters(10))
  expect_error(find_profile_filters("bad_profile"))
})

test_that("find_profile_filters returns dataframe", {
  expect_equal(ncol(find_profile_filters(2)), 2)
  expect_equal(ncol(find_profile_filters("Data licensed for all uses")), 2)
  expect_true(is(find_profile_filters("ALA"), "data.frame"))
})
