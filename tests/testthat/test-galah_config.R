context("Test galah_config")

test_that("Use of ala_config gives deprecation warning", {
  expect_warning(ala_config())
})

test_that("galah_config sets default options", {
  skip_on_cran()
  # set to null
  options(galah_config = NULL)
  # check that defaults are used
  expect_equal(galah_config()$verbose, TRUE)
})

vcr::use_cassette("logger_config", {
  test_that("galah_config checks download_id", {
    expect_error(galah_config(download_reason_id = 17))
    expect_silent(galah_config(download_reason_id = "testing"))
    expect_silent(galah_config(download_reason_id = "Testing"))
    expect_error(galah_config(download_reason_id = "tsting"))
  })
})

test_that("galah_config checks inputs", {
  expect_error(galah_config(caching = "value"))
  expect_error(galah_config(verbose = "value"))
  expect_error(galah_config(email = 4))
  expect_error(galah_config(cache_directory = "non/existent/dir"))
  expect_error(galah_config(bad_option = "value"))
  expect_error(galah_config(atlas = "world"))
  expect_silent(galah_config(atlas = "Australia"))
  expect_error(galah_config(run_checks = "value"))
  expect_silent(galah_config(run_checks = TRUE))
})