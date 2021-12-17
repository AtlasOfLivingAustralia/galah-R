context("Test search_fields")


test_that("search_fields returns nothing with blank argument", {
  skip_on_cran()
  fields <- search_fields()
  expect_equal(nrow(fields), 0)
})


test_that("search_fields returns layers", {
  skip_on_cran()
  field_layer <- search_fields("layer")
  field_layers <- search_fields("layers")
  expect_equal(unique(field_layer$type), c("fields", "layers"))
  expect_equal(unique(field_layers$type), c("fields", "layers"))
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
