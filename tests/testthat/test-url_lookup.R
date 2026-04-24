test_that("`url_lookup()` errors for nonsense strings", {
  list(type = "something", atlas = "Australia") |>
    as_query() |>
    url_lookup() |>
    expect_error(label = "No API is available")
})

test_that("`url_lookup()` returns a URL for a valid input", {
  list(type = "metadata/assertions", atlas = "Australia") |>
    as_query() |>
    url_lookup() |>
    stringr::str_detect("^https://api.ala.org.au") |>
    expect_true()
})

test_that("`url_lookup()` parses named inputs correctly", {
  list(type = "metadata/taxa-single", atlas = "Australia") |>
    as_query() |>
    url_lookup(name = "Crinia") |>
    stringr::str_detect("search\\?q=Crinia$") |>
    expect_true()
})

test_that("`url_lookup()` errors for incorrect named inputs", {
  list(type = "metadata/taxa-single", atlas = "Australia") |>
    as_query() |>
    url_lookup(something = "Crinia") |>
    expect_error()
})

test_that("`url_lookup()` parses multiple named inputs correctly", {
  list(type = "metadata/taxa-single", atlas = "Australia") |>
    as_query() |>
    url_lookup(name = c("Crinia", "Limnodynastes")) |>
    length() |>
    expect_equal(2)
})
