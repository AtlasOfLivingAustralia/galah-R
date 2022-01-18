context("Test galah_select")


test_that("galah_select returns error when columns don't exist", {
  galah_config(email = "ala4r@ala.org.au", run_checks = FALSE)
  expect_error(galah_select(basisOfRecors))
  expect_error(galah_select(year, basisOfRecord, eventdate))
})


test_that("galah_select returns requested columns", {
  galah_config(email = "ala4r@ala.org.au", run_checks = FALSE)
  selected_columns <- galah_select(year, basisOfRecord)
  query <- atlas_occurrences(identify = galah_identify("oxyopes dingo"),
                            select = selected_columns)
  expect_equal(selected_columns[[1]], c("year", "basisOfRecord"))
  expect_equal(names(query), c("year", "basisOfRecord"))
  expect_equal(names(query), selected_columns[[1]])
})


test_that("galah_select builds expected columns when group = basic", {
  # skip_on_cran()
  galah_config(email = "ala4r@ala.org.au", run_checks = FALSE)
  select <- galah_select(group = "basic")
  expected_output <- structure(
    tibble(name = c("decimalLatitude", "decimalLongitude",
                        "eventDate", "scientificName",
                        "taxonConceptID", "recordID",
                        "dataResourceName"),
               type = rep("field", times = 7)),
    class = c("tbl_df", "tbl", "data.frame", "galah_select"))
  expect_s3_class(select, "data.frame")
  expect_equal(nrow(select), nrow(expected_output))
  expect_equal(names(select), names(expected_output))
  expect_equal(select[1], expected_output[1])
  expect_equal(select[2], expected_output[2])
  galah_config(run_checks = TRUE)
})


# test_that("galah_select builds expected columns when group = fields", {
#   
# })

# test_that("galah_select builds expected columns when group = assertions", {
#   
# })


test_that("galah_select defaults to group = basic when there are no args", {
  # skip_on_cran()
  galah_config(run_checks = FALSE)
  expected_output <- structure(
    tibble(name = c("decimalLatitude", "decimalLongitude",
                        "eventDate", "scientificName",
                        "taxonConceptID", "recordID",
                        "dataResourceName"),
               type = rep("field", times = 7)),
    class = c("tbl_df", "tbl", "data.frame", "galah_select"))
  expect_s3_class(galah_select(), "data.frame")
  expect_equal(nrow(galah_select()), nrow(expected_output))
  expect_equal(names(galah_select()), names(expected_output))
  expect_equal(galah_select()[1], expected_output[1])
  galah_config(run_checks = TRUE)
})


test_that("galah_select combines requested columns and group columns", {
  galah_config(run_checks = FALSE)
  identify <- galah_identify("oxyopes dingo")
  columns <- galah_select(year, basisOfRecord, group = "basic")
  query <- atlas_occurrences(identify = identify,
                             select = columns)
  expected_columns <- c("decimalLatitude", "decimalLongitude",
                          "eventDate", "scientificName",
                          "taxonConceptID", "recordID",
                          "dataResourceName", 
                          "year", "basisOfRecord")
  expect_equal(names(query), expected_columns)
})


test_that("galah_select can use tidyselect::contains", {
  query <- galah_select(tidyselect::contains("el"))
  expect_gt(nrow(query), 0)
  expect_true(all(grepl("el", tolower(query$name))))
})

test_that("galah_select can use tidyselect::starts_with", {
  query <- galah_select(tidyselect::starts_with("el"))
  expect_gt(nrow(query), 0)
  expect_true(all(grepl("^el", tolower(query$name))))
})

test_that("galah_select can use tidyselect::last_col", {
  query <- galah_select(tidyselect::last_col())
  expect_equal(nrow(query), 1)
})

test_that("galah_select can use tidyselect::last_col & user-defined queries", {
  query <- galah_select(year, basisOfRecord, tidyselect::last_col())
  expect_equal(nrow(query), 3)
})

test_that("galah_select can use tidyselect::last_col & group", {
  query <- galah_select(tidyselect::last_col(), group = "basic")
  expect_equal(nrow(query), 8)
})