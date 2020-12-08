context("Test field function")


test_that("ala_fields works as expected", {
  expect_true(inherits(ala_fields(), "data.frame"))
  
  # expect that assertions are included
  expect_true("Assertion" %in% unique(ala_fields()$class))
})