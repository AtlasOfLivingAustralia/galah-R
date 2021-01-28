context("Find values for a field")

test_that("find_field_values returns an error for invalid fields", {
  skip_on_cran()
  expect_error(find_field_values())
  expect_error(find_field_values("bad_category"))
  expect_error(find_field_values("basis_of_record", limit = "20"))
})

test_that("find_field_values handles fields with a large number of options", {
  skip_on_cran()
  expect_warning(expect_equal(nrow(find_field_values("data_resource_uid",
                                                  limit = 10)), 10))
})

test_that("find_field_values returns expected value", {
  skip_on_cran()
  expect_equal(nrow(find_field_values("basis_of_record")), 12)
})