test_that("collect_media returns deprecated message when `path` argument is used", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  expect_error({
    media_data <- atlas_media(
      identify = galah_identify("Microseris lanceolata"),
      filter = galah_filter(year == 2019)) |>
    collect_media(path = NULL)
  })
})

test_that("collect_media suggests `galah_config(directory =)` when a temp folder is set as the directory", {
  skip_if_offline()
  atlas_query <- atlas_media(
    identify = galah_identify("Regent Honeyeater"),
    filter = galah_filter(year == 2012))
  
  media_dir <- "Temp"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  galah_config(directory = media_dir)
  expect_message(
    collect_media(atlas_query), 
    cli::cli_text("{cli::col_magenta('To change which file directory media files are saved, use `galah_config(directory = )`.')}")
    )
  unlink(media_dir, recursive = TRUE)
})

test_that("collect_media downloads images", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  media_data <- atlas_media(identify = galah_identify("Microseris lanceolata"),
                            filter = galah_filter(year == 2019))
  # full-size
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  galah_config(directory = media_dir)
  collect_media(media_data)
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data)) # correct number of images
  size_1 <- sum(file.info(
    paste0(media_dir, "/", list.files(media_dir))
  )$size)
  unlink(media_dir, recursive = TRUE)

  # thumbnail
  dir.create(media_dir)
  collect_media(media_data, thumbnail = "TRUE")
  file_count <- length(list.files(media_dir))
  expect_equal(file_count, nrow(media_data)) # correct number of images
  size_2 <- sum(file.info(
    paste0(media_dir, "/", list.files(media_dir))
  )$size)
  unlink(media_dir, recursive = TRUE) 
  
  # full-size images have smaller file size than thumbnails
  expect_lt(size_2, size_1) 
  
})

test_that("collect_media messages how many files downloaded", {
  skip_if_offline()
  atlas_query <- atlas_media(identify = galah_identify("Microseris lanceolata"),
                             filter = galah_filter(year == 2019))
  n_files <- nrow(atlas_query)
  message <- paste0("Downloaded ", n_files, " files successfully")
  
  expect_message(collect_media(atlas_query), message)
})


test_that("collect_media handles different file formats", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")

  media_data <- galah_call() |>
    galah_identify("Regent Honeyeater") |>
    galah_filter(year == 2012) |>
    atlas_media() 
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  media_files <- media_data |>
    collect_media()
  downloads <- list.files(path = media_dir)
  
  expect_true(any(grepl(".mpg$", downloads))) # videos
  expect_true(any(grepl(".jpg$", downloads))) # images
  file_count <- length(list.files(media_dir)) 
  expect_equal(file_count, nrow(media_data)) # correct n
  unlink(media_dir, recursive = TRUE)
})
