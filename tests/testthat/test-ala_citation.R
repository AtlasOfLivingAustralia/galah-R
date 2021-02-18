context("Test citations are generated")

test_that("Citation is generated for dataset with DOI", {
  skip_on_cran()
  data <- data.frame()
  attr(data, "doi") <- "test-doi"
  expect_match(ala_citation(data), "test-doi")
})

test_that("ala_citation returns an error for when no DOI exists", {
  skip_on_cran()
  data <- data.frame()
  expect_error(ala_citation(data))
})