context("Test galah_identify")

test_that("galah_identify returns an empty tibble when no args provided", {
  result <- galah_identify()
  expect_equal(nrow(result), 0)
})

test_that("galah_identify runs a search when given a string", {
  vcr::use_cassette("galah_identify_search_1", {
    result <- galah_identify("Litoria_peronii")
  })
  expect_equal(nrow(result), 1)
})

test_that("galah_identify runs a search on multiple strings", {
  vcr::use_cassette("galah_identify_search_2", {
    result <- galah_identify("amphibia", "reptilia", "aves", "mammalia")
  })
  expect_equal(nrow(result), 4)
})

test_that("galah_identify works with search = FALSE", {
  galah_config(run_checks = FALSE)
  id <- "urn:lsid:biodiversity.org.au:afd.taxon:0490a9ba-0d08-473d-a709-6c42e354f118"
  result <- galah_identify(id, search = FALSE)
  expect_equal(result$identifier[1], id)
})

test_that("galah_identify returns an error when using other atlases", {
  galah_config(atlas = "Austria")
  expect_error(galah_identify("aves"))
  galah_config(atlas = "Australia")
})

test_that("galah_identify can pass a string unchanged when run_checks = FALSE", {
  galah_config(run_checks = FALSE)
  result <- galah_identify("a_string", search = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$identifier[1], "a_string")
})

test_that("galah_identify pipes correctly", {
  vcr::use_cassette("galah_identify_search_3", {
    result <- galah_call() |>
      galah_identify("Litoria") |>
      galah_filter(year == 2020) 
  })
  expect_false(is.null(result$identify))
  expect_false(is.null(result$filter)) 
})