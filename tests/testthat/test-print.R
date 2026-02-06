test_that("object of class `data-request` formats correctly", {
  expect_snapshot(request_data())
})

test_that("populated `data_request()` prints correctly", {
  galah_call() |>
    identify("crinia") |>
    filter(year == 2025) |>
    expect_snapshot()
})

test_that("object of class `metadata-request` formats correctly", {
  request_data() |>
    expect_snapshot()
})

test_that("object of class `metadata-request` formats correctly with `filter()", {
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

test_that("object of class `query` formats correctly", {
  request_metadata() |>
    identify("Crinia") |>
    capture() |>
    expect_snapshot()
})

test_that("object of class `computed_query` formats correctly", {
  x <- request_metadata() |>
    identify("Crinia") |>
    capture()
  class(x) <- c("computed_query", "list")
  expect_snapshot(x)
})

test_that("object of class `query_set` formats correctly", {
  galah_call() |>
    filter(basisOfRecord == "HUMAN_OBSERVATION") |>
    compound() |>
    expect_snapshot()
})

test_that("`galah_config()` formats correctly", {
  galah_config(directory = "something")
  galah_config() |>
    expect_snapshot()
  galah_config(directory = tempdir(check = TRUE))
  unlink("something", recursive = TRUE)
})