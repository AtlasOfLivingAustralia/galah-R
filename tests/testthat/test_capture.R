test_that("`capture.data_request` fails for unknown `type", {
  x <- request_data()
  x$type <- "something"
  capture(x) |> expect_error()
})

test_that("`capture.metadata_request` fails for unknown `type", {
  x <- request_metadata()
  x$type <- "something"
  capture(x) |> expect_error()
})

test_that("`capture.files_request` fails for unknown `type", {
  x <- request_files()
  x$type <- "something"
  capture(x) |> expect_error()
})

test_that("`capture.list()` converts class to `prequery`", {
  list(type = "occurrences", atlas = "Global") |>
    capture() |>
    expect_s3_class("prequery")
})