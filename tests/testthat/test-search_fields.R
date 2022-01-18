context("Test search_fields")


test_that("search_fields helpful warning with blank argument", {
  skip_on_cran()
  expect_warning(search_fields(), "didn't detect a field")
})


test_that("search_fields returns assertions", {
  skip_on_cran()
  fields <- search_fields("UNKNOWN_KINGDOM")
  expect_equal(unique(fields$type), "assertions")
})


test_that("search_fields searches text correctly", {
  skip_on_cran()
  fields <- search_fields("precipitation")
  expect_true(all(grepl(pattern = "precipitation", fields$description,
                        ignore.case = TRUE)))
})
