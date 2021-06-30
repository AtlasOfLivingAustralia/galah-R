context("Test ala_config")

test_that("ala config sets default options", {
  skip_on_cran()
  # set to null
  options(galah_config = NULL)
  # check that defaults are used
  expect_equal(ala_config()$verbose, TRUE)
})

test_that("ala config checks inputs", {
  skip_on_cran()
  expect_error(ala_config(caching = "value"))
  expect_error(ala_config(verbose = "value"))
  expect_error(ala_config(email = 4))
  expect_error(ala_config(download_reason_id = 17))
  expect_silent(ala_config(download_reason_id = "testing"))
  expect_silent(ala_config(download_reason_id = "Testing"))
  expect_error(ala_config(download_reason_id = "tsting"))
  expect_error(ala_config(cache_directory = "non/existent/dir"))
  expect_error(ala_config(bad_option = "value"))
  expect_error(ala_config(atlas = "world"))
  expect_silent(ala_config(atlas = "Australia"))
})
