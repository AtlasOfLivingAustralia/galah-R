test_that("atlas_citation generates DOI for dataset with DOI", {
  df <- data.frame()
  attr(df, "doi") <- "test-doi"
  x <- atlas_citation(df) |>
    suppressWarnings()
  expect_equal(x,
               c("The supplied DOI was not recognized.",
                 i = "Please consider checking the atlas in question for their citation guidelines"))
})

test_that("atlas_citation returns an error when no DOI exists", {
  data <- data.frame()
  atlas_citation(data) |>
    expect_warning()
  x <- atlas_citation(data) |>
    suppressWarnings()
  expect_equal(x,
               c(
                 "This dataset does not have any citation information attached.",
                 i = "Please consider checking the atlas in question for their citation guidelines"
               ))
})

test_that("atlas_citation attributes ALA DOIs correctly", {
  df <- data.frame()
  attr(df, "doi") <- "https://doi.org/10.26197/ala.68d1695a-83dd-45bf-88a5-7b65e7fc1553"
  citation <- atlas_citation(df) |>
    suppressMessages()
  expect_true(grepl("Atlas of Living Australia", citation))
})

test_that("atlas_citation attributes GBIF DOIs correctly", {
  df <- data.frame()
  attr(df, "doi") <- "10.15468/dl.randomstring"
  citation <- atlas_citation(df) |>
    suppressMessages()
  expect_true(grepl("GBIF Occurrence Download", citation))
})

test_that("atlas_citation works on a real download", {
  skip_if_offline(); skip_on_ci()
  galah_config(email = "ala4r@ala.org.au")
  x <- galah_call() |>
    identify("Heleioporus") |>
    filter(year == 2022) |>
    collect()
  text_out <- atlas_citation(x) |>
    suppressMessages()
  grepl("^The citation for this dataset is:", text_out) |>
    expect_true()
  grepl("Please consider citing R & galah", text_out) |>
    expect_true()
})