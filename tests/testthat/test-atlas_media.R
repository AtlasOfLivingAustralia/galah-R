test_that("`collapse()` and `collect()` work for `type = 'media'`", {
  ## PART 1: request data
  galah_config(email = "ala4r@ala.org.au")
  x <- request_data(type = "media") |>
    filter(year == 2010, !is.na(images)) |>
    select(group = "media") |>  # c("basic", "media")) |>
    identify("Litoria peronii") |>
    collapse()
  expect_true(inherits(x, "query_set"))
  expect_equal(length(x), 6)
  expect_true(all(
    unlist(lapply(x, function(a){a$type})) %in%
      c("metadata/fields", 
        "metadata/assertions", 
        "metadata/reasons", 
        "metadata/taxa-single",
        "data/occurrences", 
        "data/media")))
  skip_if_offline()
  # compute stage
  y <- compute(x)
  expect_true(inherits(y, "query"))
  expect_equal(length(y), 5)
  expect_equal(names(y), 
               c("type", "url", "body", "headers", "data/occurrences"))
  # collect stage
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(z), 1)
  expect_gte(ncol(z), 6) # number of fields requested by `select()`
  
  ## PART 2: request values
  a <- request_files(type = "media") |>
    filter(urls == z$image_url) |>
    collapse()
  # NOT YET FUNCTIONAL
})


test_that("atlas_media gives a warning when old arguments are used", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au", atlas = "Australia")
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
  skip_if_offline()
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
  skip_if_offline()
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
