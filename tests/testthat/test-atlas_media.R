test_that("`collapse()` and `collect()` work for `type = 'media'`: tibble stage", {
  ## PART 1: request data
  galah_config(email = "ala4r@ala.org.au")
  media_collapse <- request_data(type = "media") |>
    filter(year == 2010, !is.na(images)) |>
    select(group = "media") |>  # c("basic", "media")) |>
    identify("Litoria peronii") |>
    collapse()
  expect_true(inherits(media_collapse, "query_set"))
  expect_equal(length(media_collapse), 6)
  expect_true(all(
    unlist(lapply(media_collapse, function(a){a$type})) %in%
      c("metadata/fields", 
        "metadata/assertions", 
        "metadata/reasons", 
        "metadata/taxa-single",
        "data/occurrences", 
        "data/media")))
  skip_if_offline()
  # compute stage
  media_compute <- compute(x)
  expect_true(inherits(media_compute, "query"))
  expect_equal(length(media_compute), 5)
  expect_equal(names(media_compute), 
               c("type", "url", "body", "headers", "data/occurrences"))
  # collect stage
  media_collect <- collect(media_compute)
  expect_s3_class(media_collect, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(media_collect), 1)
  expect_gte(ncol(media_collect), 6) # number of fields requested by `select()`
})

test_that("collapse() and compute() work for `type = 'media'`: files stage", {
  ## PART 2: request values
  media_records <- request_data(type = "media") |>
    filter(year == 2010, !is.na(images)) |>
    select(group = "media") |>  # c("basic", "media")) |>
    identify("Litoria peronii") |>
    collect() |>
    slice_head(n = 3)
  
  media_dir <- "test_media"
  unlink(media_dir, recursive = TRUE)
  dir.create(media_dir)
  galah_config(directory = media_dir)
  files_collapse <- request_files(type = "media") |>
    filter(media == slice_head(media_records, n = 3)) |>
    collapse()
  expect_true(inherits(files_collapse, "query_set"))
  expect_equal(length(files_collapse), 1)
  expect_true(all(
    unlist(lapply(files_collapse, function(a){a$type})) %in%
      c("files/media")))
  
  # compute extracts the query 
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
