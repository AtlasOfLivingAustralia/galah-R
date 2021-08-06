context("Test deletion of cached files and cached file metadata")

test_that("clear_cached_files gives an error if no metadata file exists", {
  skip_on_cran()
  galah_config(caching = TRUE, cache_directory = tempdir())
  # make sure there is no metadata file in directories
  suppressWarnings(file.remove(file.path(tempdir(), 'metadata.rds')))
  expect_message(clear_cached_files())
  galah_config(caching = FALSE)
})
