test_that("galah_config sets default options", {
  skip_on_cran()
  # check that defaults are used
  expect_equal(galah_config()$package$verbose, TRUE)
})

test_that("galah_config checks download_id", {
  skip_if_offline()
  galah_config(verbose = TRUE)
  expect_error(galah_config(download_reason_id = 17))
  expect_error(galah_config(download_reason_id = "NOTHING"))
  expect_silent(galah_config(download_reason_id = 3))
  expect_silent(galah_config(download_reason_id = "education"))
  galah_config(verbose = FALSE)
})

test_that("galah_config checks inputs", {
  skip_if_offline()
  expect_error(galah_config(caching = "value"))
  expect_error(galah_config(verbose = "value"))
  expect_error(galah_config(email = 4))
  expect_error(galah_config(cache_directory = "non/existent/dir"))
  expect_error(galah_config(bad_option = "value"))
  expect_error(galah_config(atlas = "world"))
  expect_silent(galah_config(verbose = FALSE, atlas = "Australia"))
  expect_error(galah_config(run_checks = "value"))
  expect_silent(galah_config(run_checks = TRUE))
})
