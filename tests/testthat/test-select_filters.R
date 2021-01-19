context("Test ALA filters")

test_that("select_filters builds data quality filters", {
  expect_s3_class(select_filters(profile = "ALA"),
                  "data.frame")
  expect_error(select_filters(profile = "bad"))
  expect_equal(unique(select_filters(profile = "CSDM")$include),
               c(TRUE, FALSE))
})

test_that("select_filters handles assertion filters", {
  expect_true("assertions" %in%
                select_filters(list(zeroCoordinates = FALSE,
                                 habitatMismatch = FALSE))$name)
})

test_that("select_filters handles exclusion filters", {
  expect_false(select_filters(list(basis_of_record =
                                  exclude("HumanObservation")))$include)
})

test_that("select_filters validates filters", {
  expect_error(select_filters(invalid_filter = 'value'))
})

test_that("select_filters converts logical to string", {
  expect_equal(unlist(select_filters(list(geospatial_kosher = TRUE))$value),
               "true")
})
