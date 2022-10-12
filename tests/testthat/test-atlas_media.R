context("Test atlas_media")
teardown(unlink("test_media", recursive = TRUE))

test_that("atlas_media gives a warning when old arguments are used", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  expect_message({
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
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  media_data <- atlas_media(
    identify = galah_identify("Microseris lanceolata"),
    filter = galah_filter(year == 2019))
  expect_s3_class(media_data, c("tbl_df", "tbl", "data.frame"))
  file_count <- length(list.files(media_dir))
  expect_lt(file_count, 1)  # no files in specified directory
  unlink(media_dir, recursive = TRUE)
})

test_that("atlas_occurrences |> search_media duplicates atlas_media", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au")

  media_1 <- atlas_occurrences(
     identify = galah_identify("Microseris lanceolata"),
     filter = galah_filter(year == 2019, multimedia == "Image"),
     select = galah_select(group = c("basic", "media"))) |>
     search_media()

  media_2 <- atlas_media(
     identify = galah_identify("Microseris lanceolata"),
     filter = galah_filter(year == 2019))

  expect_equal(nrow(media_1), nrow(media_2))
})
