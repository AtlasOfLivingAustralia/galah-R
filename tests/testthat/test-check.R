test_that("check_download_filename() works", {
  # missing case (default)
  galah_config(directory = "temp")
  x <- check_download_filename(file = NULL)
  expect_true(grepl(galah_config()$package$directory, x)) # contains directory
  expect_true(grepl(".zip$", x))
  # file name with no suffix
  x <- check_download_filename(file = "something")
  expect_true(grepl(galah_config()$package$directory, x)) # contains directory
  expect_true(grepl("something.zip$", x))
  # wrong suffix
  x <- check_download_filename(file = "something.csv")
  expect_true(grepl(galah_config()$package$directory, x)) # contains directory
  expect_true(grepl("something.zip$", x))
  unlink("temp")
})