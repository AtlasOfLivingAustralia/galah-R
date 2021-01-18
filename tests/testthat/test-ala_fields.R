context("Test field function")


test_that("find_fields works as expected", {
  expect_true(inherits(find_fields(), "data.frame"))
  
  # expect that assertions are included
  expect_true("Assertion" %in% unique(find_fields()$class))
})