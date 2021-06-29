context("Find values for a field")

test_that("find_field_values returns an error for invalid fields", {
  skip_on_cran()
  expect_error(find_field_values())
  expect_error(find_field_values("bad_category"))
  expect_error(find_field_values("basis_of_record", limit = "20"))
})

test_that("find_field_values handles fields with a large number of options", {
  skip_on_cran()
  expect_warning(expect_equal(nrow(find_field_values("dataResourceUid",
                                                  limit = 10)), 10))
})

test_that("find_field_values returns expected value", {
  skip_on_cran()
  expect_gt(nrow(find_field_values("basisOfRecord")), 7)
})

test_that("find_field_values returns a warning for lots of fields", {
  skip_on_cran()
  expect_warning(find_field_values("year"))
})