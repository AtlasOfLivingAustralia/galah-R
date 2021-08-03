context("Test deletion of cached files and cached file metadata")

test_that("clear_cached_files gives an error if no metadata file exists", {
  skip_on_cran()
  ala_config(cache_directory = tempdir())
  # make sure there is no metadata file in directorys
  suppressWarnings(file.remove(file.path(tempdir(), 'metadata.rds')))
  expect_message(clear_cached_files())
})