test_that("atlas_citation generates DOI for dataset with DOI", {
  df <- data.frame()
  attr(df, "doi") <- "test-doi"
  expect_match(atlas_citation(df), "test-doi")
})

test_that("atlas_citation returns an error when no DOI exists", {
  data <- data.frame()
  expect_warning(atlas_citation(data))
})

test_that("atlas_citation attributes ALA DOIs correctly", {
  df <- data.frame()
  attr(df, "doi") <- "https://doi.org/10.26197/ala.68d1695a-83dd-45bf-88a5-7b65e7fc1553"
  citation <- atlas_citation(df)
  expect_true(grepl("^Atlas of Living Australia", citation))
})

test_that("atlas_citation attributes GBIF DOIs correctly", {
  df <- data.frame()
  attr(df, "doi") <- "https://doi.org/10.15468/dl.2q87rx"
  citation <- atlas_citation(df)
  expect_true(grepl("^GBIF.org", citation))
})