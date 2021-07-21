context("Test retrieval of cached file metadata")

test_that("find_cached_files gives an error if no metadata file exists", {
  skip_on_cran()
  ala_config(cache_directory = tempdir())
  # make sure there is no metadata file in directorys
  suppressWarnings(file.remove(file.path(tempdir(), 'metadata.rds')))
  expect_message(find_cached_files())
})

test_that("find_cached_files reads metadata", {
  # create some metadata
  ala_config(caching = TRUE)
  ala_counts(group_by = "year")
  expect_type(find_cached_files(), "list")
  expect_s3_class(find_cached_files()[[1]]$data_request, "data_request")
})