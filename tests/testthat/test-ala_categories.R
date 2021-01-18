context("Find ALA categories")

test_that("find_categories returns an error for invalid fields", {
  skip_on_cran()
  expect_error(find_categories())
  expect_error(find_categories("bad_category"))
  expect_error(find_categories("basis_of_record", limit = "20"))
})

test_that("find_categories handles fields with a large number of options", {
  skip_on_cran()
  expect_warning(expect_equal(nrow(find_categories("data_resource_uid",
                                                  limit = 10)), 10))
})

test_that("find_categories returns expected value", {
  skip_on_cran()
  expect_equal(nrow(find_categories("basis_of_record")), 12)
})