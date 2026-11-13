test_that("`query_API()` fails nicely for invalid API calls", {
  # start with a valid call
  x <- galah_call() |>
    filter(year == 2020) |>
    count() |>
    collapse()
  # replace with an intentionally wrong string
  x$url <- stringr::str_replace(x$url,
                                "occurrences/occurrences",
                                "occurrences/nothing")
  # use internal functions
  collect(x) |>
    expect_error(label = "HTTP 403 Forbidden")
})
