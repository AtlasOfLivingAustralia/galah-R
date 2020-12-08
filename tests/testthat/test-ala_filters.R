context("Test ALA filters")

test_that("ala_filters builds data quality filters", {
  expect_s3_class(ala_filters(data_quality_profile = "ALA"),
                  "data.frame")
  expect_error(ala_filters(data_quality_profile = "bad"))
  expect_equal(unique(ala_filters(data_quality_profile = "CSDM")$include),
               c(TRUE, FALSE))
})

test_that("ala_filters handles assertion filters", {
  expect_true("assertions" %in%
                ala_filters(list(zeroCoordinates = FALSE,
                                 habitatMismatch = FALSE))$name)
})

test_that("ala_filters handles exclusion filters", {
  expect_false(ala_filters(list(basis_of_record =
                                  exclude("HumanObservation")))$include)
})

test_that("ala filters validates filters", {
  expect_error(ala_filters(invalid_filter = 'value'))
})

test_that("ala filters converts logical to string", {
  expect_equal(unlist(ala_filters(list(geospatial_kosher = TRUE))$value),
               "true")
})
