
test_that("`query_API()` fails nicely for wrong base url (curl error)", {
  # start with a valid call
  x <- galah_call() |>
    filter(year == 2020) |>
    count() |>
    collapse()
  # replace with an intentionally wrong string
  y <- httr2::url_parse(x$url)
  y$hostname <- "nothing.ala.org.au"
  x$url <- httr2::url_build(y)
  # check errors correctly
  collect(x) |>
    expect_error(label = "Failed to perform HTTP request.")
})

test_that("`query_API()` fails nicely for wrong extention (code 403)", {
  # start with a valid call
  x <- galah_call() |>
    filter(year == 2020) |>
    count() |>
    collapse()
  # replace with an intentionally wrong string suffix
  x$url <- stringr::str_replace(x$url,
                                "occurrences/occurrences",
                                "occurrences/nothing")
  # use internal functions
  collect(x) |>
    expect_error(label = "HTTP Code 403: Missing Authentication Token")
})
