context("Test layer retrieval")

test_that("find_layers returns expected columns", {
  expect_equal(ncol(find_layers()), 4)
})