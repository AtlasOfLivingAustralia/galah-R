test_that("`url_lookup()` errors for nonsense strings", {
  url_lookup("something") |>
    expect_error(label = "No API is available")
})

test_that("`url_lookup()` returns a URL for a valid input", {
  url_lookup("metadata/assertions") |>
    stringr::str_detect("^https://api.ala.org.au") |>
    expect_true()
})

test_that("`url_lookup()` parses named inputs correctly", {
  url_lookup("metadata/taxa-single", 
             name = "Crinia") |>
    stringr::str_detect("search\\?q=Crinia$") |>
    expect_true()
})

test_that("`url_lookup()` errors for incorrect named inputs", {
  url_lookup("metadata/taxa-single", 
             something = "Crinia") |>
    expect_error()
})

test_that("`url_lookup()` parses multiple named inputs correctly", {
  url_lookup("metadata/taxa-single", 
             name = c("Crinia", "Limnodynastes")) |>
    length() |>
    expect_equal(2)
})
