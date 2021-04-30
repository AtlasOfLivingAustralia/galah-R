context("Test ALA filters")

test_that("select_filters builds data quality filters", {
  skip_on_cran()
  expect_s3_class(select_filters(profile = "ALA"),
                  "data.frame")
  expect_error(select_filters(profile = "bad"))
  expect_equal(unique(select_filters(profile = "CSDM")$include),
               c(TRUE, FALSE))
})

test_that("select_filters handles assertion filters", {
  skip_on_cran()
  expect_true("assertions" %in%
                select_filters(zeroCoordinates = FALSE,
                                 habitatMismatch = FALSE)$name)
})

test_that("select_filters handles exclusion filters", {
  skip_on_cran()
  expect_false(select_filters(basis_of_record =
                                  exclude("HumanObservation"))$include)
})

test_that("select_filters validates filters", {
  skip_on_cran()
  expect_error(select_filters(invalid_filter = 'value'))
})

test_that("select_filters converts logical to string", {
  skip_on_cran()
  expect_equal(unlist(select_filters(geospatial_kosher = TRUE)$value),
               "true")
})
