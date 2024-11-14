test_that("galah_config warns that `cache_directory` is deprecated", {
  unlink("temp", recursive = TRUE)
  dir.create("temp")
  expect_warning(galah_config(cache_directory = "temp"))
  expect_true(galah_config()$package$directory == "temp")
  galah_config(directory = tempfile())
  unlink("temp", recursive = TRUE)
})

test_that("galah_config creates nested folders where requested", {
  galah_config(directory = "non/existent")
  directories <- list.dirs(recursive = TRUE)
  expect_true(all(c("./non", "./non/existent") %in% directories))
  galah_config(directory = tempfile())
  unlink("non", recursive = TRUE)
})

test_that("galah_config checks download_id", {
  skip_if_offline()
  galah_config(verbose = TRUE)
  expect_error(galah_config(download_reason_id = 17))
  expect_error(galah_config(download_reason_id = "NOTHING"))
  expect_silent(galah_config(download_reason_id = 3))
  expect_message(galah_config(download_reason_id = "education"))
  expect_equal(galah_config()$user$download_reason_id, 3)
  galah_config(verbose = FALSE)
})

test_that("galah_config checks inputs", {
  skip_if_offline()
  expect_error(galah_config(caching = "value"))
  expect_error(galah_config(verbose = "value"))
  expect_error(galah_config(email = 4))
  expect_error(galah_config(bad_option = "value"))
  expect_error(galah_config(atlas = "world"))
  expect_silent(galah_config(verbose = FALSE, atlas = "Australia"))
  expect_error(galah_config(run_checks = "value"))
  expect_silent(galah_config(run_checks = TRUE))
  # reset defaults 
  galah_config(run_checks = TRUE,
               verbose = FALSE,
               download_reason_id = "testing")
})

test_that("galah_config can swap between atlases", {
  expect_message(galah_config(atlas = "GBIF"))
  x <- galah_config()
  expect_equal(x$atlas, 
               list(organisation = "Global Biodiversity Information Facility",
                    acronym = "GBIF",
                    region = "Global"))
  galah_config(atlas = "ALA")
})