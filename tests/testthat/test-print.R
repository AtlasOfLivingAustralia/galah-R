# class `data_request`

test_that("Empty `data-request` formats correctly", {
  list() |>
    structure(class = c("data_request", "list")) |>
    expect_snapshot()
})

test_that("object of class `data-request` formats correctly", {
  request_data() |>
    expect_snapshot()
})

test_that("populated `data_request()` prints correctly", {
  galah_call() |>
    identify(c("crinia", "uperoleia")) |>
    filter(year == 2025) |>
    group_by(species) |>
    count() |>
    select(species, count) |>
    expect_snapshot()
})

# class `metadata_request`

test_that("object of class `metadata-request` formats correctly", {
  request_metadata() |>
    expect_snapshot()
})

test_that("populated `metadata_request()` prints correctly", {
  request_metadata() |>
    filter(list == "dr650") |>
    expect_snapshot()
})

test_that("object of class `metadata-request` formats correctly with `identify()", {
  request_metadata() |>
    identify("Crinia") |>
    expect_snapshot()
})

test_that("object of class `metadata-request` formats correctly with `identify() |> unnest()", {
  request_metadata() |>
    identify("Crinia") |>
    unnest() |>
    expect_snapshot()
})

# class `files_request`

test_that("object of class `files-request` formats correctly", {
  request_files() |>
    expect_snapshot()
})

test_that("populated `files_request()` prints correctly", {
  request_files() |>
    filter(media == tibble::tibble(media_id = 1234)) |>
    expect_snapshot()
})

# class `query`

test_that("object of class `query` formats correctly", {
  request_metadata() |>
    identify("Crinia") |>
    capture() |>
    expect_snapshot()
})

test_that("Object of class `query` containing `slice_head()` formats correctly", {
  galah_call() |>
    filter(year == 2025) |>
    count(basisOfRecord) |>
    slice_head(n = 3) |>
    arrange(basisOfRecord) |>
    capture() |>
    as_query() |> 
    expect_snapshot()
  # test that slice, arrange etc are actually printed
})

# class `query_set`

test_that("object of class `query_set` formats correctly", {
  # prevent earlier caching (or lack thereof) affecting the result
  reset_cache()
  galah_config(run_checks = TRUE) # necessary status affects whether `logger` is run,
  # which affects the test below
  galah_call() |>
    authenticate(email = "ala4r@ala.org.au") |>
    filter(basisOfRecord == "HUMAN_OBSERVATION") |>
    compound() |>
    expect_snapshot()
})

# class `computed_query`

test_that("object of class `computed_query` formats correctly", {
  x <- request_metadata() |>
    identify("Crinia") |>
    capture()
  class(x) <- c("computed_query", "list")
  expect_snapshot(x)
})

# class `galah_config`

test_that("`galah_config()` formats correctly", {
  galah_config(atlas = "ALA",
               email = "random_email_for_testing@email.com",
               directory = "something",
               verbose = FALSE,
               caching = TRUE,
               run_checks = TRUE,
               download_reason_id = 10)
  galah_config() |>
    expect_snapshot()
  galah_config(directory = tempdir(check = TRUE))
  unlink("something", recursive = TRUE)
})