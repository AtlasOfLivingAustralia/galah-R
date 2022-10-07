context("Test search_values")

# test input checks on type
# search_values returns a tibble (and returns nothing when nothing is found)
# show_values works for fields, profiles and lists

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

test_that("search_profile_attributes checks input", {
  expect_error(search_profile_attributes(10))
  expect_error(search_profile_attributes("invalid"))
})

test_that("search_profile_attributes returns dataframe", {
  skip_on_cran()
  atts <- search_profile_attributes(92)
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, c("tbl_df", "tbl", "data.frame"))
  atts <- search_profile_attributes("ALA")
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, c("tbl_df", "tbl", "data.frame"))
  atts <- search_profile_attributes("ALA General")
  expect_equal(ncol(atts), 2)
  expect_s3_class(atts, c("tbl_df", "tbl", "data.frame"))
})