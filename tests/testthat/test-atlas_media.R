context("Test atlas_media")
teardown(unlink("test_media", recursive = TRUE))

test_that("atlas_media check inputs", {
  skip_on_cran()
  expect_error(atlas_media())
  expect_error(atlas_media(identify = galah_identify("Microseris lanceolata"),
                           download_dir = "non_existent"))
})

test_that("atlas_media downloads images", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  media_data <- atlas_media(identify = galah_identify("Microseris lanceolata"),
                            filter = galah_filter(year == 2019),
                            download_dir = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data))
  
  unlink(media_dir, recursive = TRUE)
})

test_that("atlas_media handles different file formats", {
  skip_on_cran()
  media_dir <- "test_media"
  dir.create(media_dir)
  media_data <- atlas_media(identify = galah_identify("Regent Honeyeater"),
                            filter = galah_filter(year == 2012),
                            download_dir = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data))
  unlink(media_dir, recursive = TRUE)
})
