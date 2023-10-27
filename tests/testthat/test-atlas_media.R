test_that("atlas_media fails when no filters are provided", {
  expect_error(atlas_media())
})

test_that("`atlas_media()` works", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  media_data <- galah_call() |>
    identify("Microseris lanceolata") |>
    filter(year == 2019) |>
    atlas_media()
  expect_s3_class(media_data, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(media_data), 3)
  expect_gte(ncol(media_data), 3)
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

test_that("`collapse()` and `collect()` work for `type = 'media'`", {
  skip_if_offline()
  ## PART 1: request occurrence data
  galah_config(email = "ala4r@ala.org.au")
  occ_collect <- request_data() |>
    identify("Litoria peronii") |>
    filter(year == 2010, !is.na(images)) |>
    select(group = "media") |>
    collect(wait = TRUE)
  
  ## PART 2: request media metadata
  # collapse
  media_collapse <- request_metadata() |>
    filter(media == occ_collect) |>
    collapse()
  expect_true(inherits(media_collapse, "query_set"))
  expect_equal(length(media_collapse), 1)
  expect_true(media_collapse[[1]]$type == "metadata/media")
  # compute
  media_compute <- compute(media_collapse)
  expect_true(inherits(media_compute, "query"))
  expect_equal(length(media_compute), 4)
  expect_equal(names(media_compute), c("type", "url", "body", "headers"))
  # collect
  media_collect <- collect(media_compute)
  expect_s3_class(media_collect, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(media_collect), 1)
  expect_gte(ncol(media_collect), 6)
  
  # PART 3: get images
  # set up directory for testing purposes
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  galah_config(directory = media_dir)
  # collapse
  df <- slice_head(media_collect, n = 3)
  files_collapse <- request_files() |>
    filter(media == df) |>
    collapse(thumbnail = TRUE)
  expect_true(inherits(files_collapse, "query_set"))
  expect_equal(length(files_collapse), 1)
  expect_equal(files_collapse[[1]]$type, "files/media")
  # compute
  files_compute <- compute(files_collapse)
  expect_true(inherits(files_compute, "query"))
  expect_equal(length(files_compute), 3)
  expect_equal(names(files_compute), c("type", "url", "headers"))
  # collect
  files_collect <- collect(files_compute)
  expect_s3_class(files_collect, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(files_collect), 1)
  expect_gte(ncol(files_collect), 2)
  expect_equal(length(list.files(media_dir)), 3)
  
  # PART 4: check collect_media()
  expect_error(collect_media(df, path = NULL)) # check gives error when `path` is set
  expect_message(collect_media(df),
                 "Downloaded 3 files successfully")
  jpg_files <- list.files(media_dir, pattern = ".jpg$|.jpeg$")
  fullsize <- sum(file.info(paste0(media_dir, "/", jpg_files))$size)
  collect_media(df, thumbnail = TRUE)
  jpg_files <- list.files(media_dir, pattern = ".jpg$|.jpeg$")
  thumbsize <- sum(file.info(paste0(media_dir, "/", jpg_files))$size)
  expect_lt(thumbsize, fullsize)
  unlink(media_dir, recursive = TRUE)
})

test_that("atlas_media gives a warning when old arguments are used", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au", atlas = "Australia")
  expect_error({
    media_data <- atlas_media(
      identify = galah_identify("Microseris lanceolata"),
      filter = galah_filter(year == 2019),
      download_dir = "test")
  })
})

test_that("collect_media handles different file formats", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  media_data <- galah_call() |>
    galah_identify("Regent Honeyeater") |>
    galah_filter(year == 2012) |>
    atlas_media() 
  expect_equal(sort(unique(media_data$multimedia)),
               c("Image", "Sound"))
  collect_media(media_data)
  downloads <- list.files(path = media_dir)
  expect_true(any(grepl(".mpg$", downloads))) # sound
  expect_true(any(grepl(".jpg$", downloads))) # images
  file_count <- length(list.files(media_dir)) 
  # expect_equal(file_count, nrow(media_data)) # FIXME correct n
  unlink(media_dir, recursive = TRUE)
})

# clean up
unlink("test_media", recursive = TRUE)
unlink("Temp", recursive = TRUE)
cache_dir <- tempfile()
dir.create(cache_dir)
galah_config(directory = cache_dir)
rm(cache_dir)