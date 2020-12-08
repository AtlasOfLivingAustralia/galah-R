context("Test layer retrieval")

test_that("ala layers returns expected columns", {
  expect_equal(ncol(ala_layers()), 4)
})