context("Test ALA media")
teardown(unlink("test_media"))

test_that("ala media check inputs", {
  image_id <- "24fbddc4-a95e-4e97-b588-e76321c9cc73"
  expect_error(ala_media())
  expect_error(ala_media(identifier = image_id,
                         identifier_type = "bad_type"))
  expect_error(ala_media(identifier = 1))
  expect_error(ala_media(image_id,
               download_dir = "bad_path"))
  expect_error(ala_media(image_id))
  
  
})

test_that("ala media handles a single image id", {
  skip_on_cran()
  media_dir <- "test_media"
  unlink(media_dir)
  dir.create(media_dir)
  expect_error(ala_media(identifier = "bad_id", identifier_type = "media",
                         download_dir = media_dir))
  media_data <- ala_media(identifier = "24fbddc4-a95e-4e97-b588-e76321c9cc73",
                          identifier_type = "media", download_dir = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data))
  
  unlink(media_dir, recursive = TRUE)
})

test_that("ala media handles multiple image ids", {
  skip_on_cran()
  media_dir <- "test_media"
  dir.create(media_dir)
  media_data <- ala_media(
    identifier = c("24fbddc4-a95e-4e97-b588-e76321c9cc73",
                   "61237df1-1939-44d3-b63a-c09213f9a261"),
                          identifier_type = "media", download_dir = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data))
  unlink(media_dir, recursive = TRUE)
})

test_that("ala media handles a single occurrence id", {
  skip_on_cran()
  media_dir <- "test_media"
  dir.create(media_dir)
  media_data <- ala_media(identifier = "80b1d0d6-2ca5-475a-a014-302f05d51839",
                          identifier_type = "occurrence",
                          download_dir = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data))
  unlink(media_dir, recursive = TRUE)
  
})

test_that("ala media handles a multiple occurrence ids", {
  skip_on_cran()
  media_dir <- "test_media"
  dir.create(media_dir)
  media_data <- ala_media(
    identifier = c("80b1d0d6-2ca5-475a-a014-302f05d51839",
                  "0053e37a-4bf7-4f6d-be2d-e66645b668c6"),
                          identifier_type = "occurrence",
    download_dir = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data))
  unlink(media_dir, recursive = TRUE)
  
})