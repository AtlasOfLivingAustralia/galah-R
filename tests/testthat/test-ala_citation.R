context("Test citations are generated")

test_that("Citation is generated for dataset with DOI", {
  skip_on_cran()
  data <- data.frame()
  attr(data, "doi") <- "test-doi"
  expect_match(ala_citation(data), "test-doi")
})

test_that("ala_citation returns an error when no DOI or search url exists", {
  skip_on_cran()
  data <- data.frame()
  attr(data, "doi") <- NA
  attr(data, "search_url") <- NA
  expect_error(ala_citation(data))
})

test_that("ala citation prodcues a citation using a search url", {
  skip_on_cran()
  data <- data.frame()
  attr(data, "doi") <- NA
  attr(data, "search_url") <- "test_url"
  expect_true(grepl("test_url", ala_citation(data)))
})