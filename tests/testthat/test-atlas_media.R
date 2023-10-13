test_that("`collapse()` and `collect()` work for `type = 'media'`", {
  skip_if_offline()
  ## PART 1: request occurrence data
  galah_config(email = "ala4r@ala.org.au")
  occ_collect <- request_data() |>
    filter(year == 2010, !is.na(images)) |>
    select(group = "media") |>
    identify("Litoria peronii") |>
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
  files_collapse <- request_files() |>
    filter(media == slice_head(media_collect, n = 3)) |>
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

test_that("atlas_media fails when no filters are provided", {
  expect_error(atlas_media())
})

test_that("atlas_media returns a tibble, and nothing else", {
  skip_if_offline()
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  galah_config(email = "ala4r@ala.org.au", 
               directory = media_dir)
  media_data <- galah_call() |>
    identify("Microseris lanceolata") |>
    filter(year == 2019) |>
    atlas_media()
  expect_s3_class(media_data, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(media_data), 3)
  expect_gte(ncol(media_data), 3)
  collect_media(media_data[1:3, ], thumbnail = TRUE)
  all_files <- list.files(media_dir)
  jpg_files <- all_files[grepl(".jpg$|.jpeg$", all_files)]
  expect_equal(length(jpg_files), 3)
  unlink(media_dir, recursive = TRUE)
})
