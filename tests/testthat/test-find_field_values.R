context("Find values for a field")

test_that("find_field_values returns an error for empty call", {
  expect_error(find_field_values())
})

vcr::use_cassette("find_field_value_invalid", {
  test_that("find_field_values returns error for invalid fields", {
    expect_error(find_field_values("bad_category"))
    expect_error(find_field_values("basis_of_record", limit = "20"))
  })
})

vcr::use_cassette("find_field_values", {
  test_that("find_field_values handles fields with lots of options", {
    expect_warning(vals <- find_field_values("dataResourceUid", limit = 10))
    expect_equal(nrow(vals), 10)
    expect_equal(names(vals), c("field", "category"))
  })
})