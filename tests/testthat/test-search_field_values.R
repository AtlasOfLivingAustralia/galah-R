context("Test search_field_values")

test_that("search_field_values returns an error for empty call", {
  expect_error(search_field_values())
})

vcr::use_cassette("search_field_value_invalid", {
  test_that("search_field_values returns error for invalid fields", {
    expect_error(search_field_values("bad_category"))
    expect_error(search_field_values("basis_of_record", limit = "20"))
  })
})

vcr::use_cassette("search_field_values", {
  test_that("search_field_values handles fields with lots of options", {
    galah_config(verbose = TRUE)
    expect_warning(vals <- search_field_values("dataResourceUid", limit = 10))
    expect_equal(nrow(vals), 10)
    expect_equal(names(vals), c("field", "category"))
    galah_config(verbose = FALSE)
  })
})