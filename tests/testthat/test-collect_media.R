test_that("collect_media fails when `path` is not set", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  expect_error({
    media_data <- atlas_media(
      identify = galah_identify("Microseris lanceolata"),
      filter = galah_filter(year == 2019)) |>
    collect_media(path = NULL)
  })
})

test_that("collect_media fails when `path` doesn't exist", {
  skip_on_cran()
  atlas_query <- atlas_media(
    identify = galah_identify("Microseris lanceolata"),
    filter = galah_filter(year == 2021))
  expect_error(collect_media(atlas_query, path = "non_existent"))
})

test_that("collect_media downloads images", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  media_data <- atlas_media(identify = galah_identify("Microseris lanceolata"),
                            filter = galah_filter(year == 2019))
                            
  # full-size
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  collect_media(media_data, path = media_dir)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data)) # correct number of images
  size_1 <- sum(file.info(
    paste0(media_dir, "/", list.files(media_dir))
  )$size)
  unlink(media_dir, recursive = TRUE)

  # thumbnail
  dir.create(media_dir)
  collect_media(media_data, path = media_dir, type = "thumbnail")
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data)) # correct number of images
  size_2 <- sum(file.info(
    paste0(media_dir, "/", list.files(media_dir))
  )$size)
  unlink(media_dir, recursive = TRUE) 
  
  # full-size images have smaller file size than thumbnails
  expect_lt(size_2, size_1) 
  
})

test_that("collect_media handles different file formats", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  media_dir <- "test_media"
  dir.create(media_dir)
  media_data <- atlas_media(identify = galah_identify("Regent Honeyeater"),
                            filter = galah_filter(year == 2012)) |>
                collect_media(path = media_dir)
  expect_true(any(grepl(".mpg$", media_data$download_path))) # videos
  expect_true(any(grepl(".jpg$", media_data$download_path))) # images
  file_count <- length(list.files(media_dir)) 
  expect_equal(file_count, nrow(media_data)) # correct n
  unlink(media_dir, recursive = TRUE)
})