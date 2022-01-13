context("Test that citations are generated")

test_that("atlas_citation generates DOI for dataset with DOI", {
  data <- data.frame()
  attr(data, "doi") <- "test-doi"
  expect_match(atlas_citation(data), "test-doi")
})

test_that("atlas_citation returns an error when no DOI or search url exists", {
  data <- data.frame()
  attr(data, "doi") <- NA
  attr(data, "search_url") <- NA
  expect_error(atlas_citation(data))
})

test_that("atlas_citation produces a citation using a search url", {
  data <- data.frame()
  attr(data, "doi") <- NA
  attr(data, "search_url") <- "test_url"
  expect_true(grepl("test_url", atlas_citation(data)))
})