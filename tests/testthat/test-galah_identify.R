context("Test galah_identify")

test_that("galah_identify returns an empty tibble when no args provided", {
  result <- galah_identify()
  expect_equal(nrow(result), 0)
})

test_that("galah_identify runs a search when given a string", {
  capture_requests("galah_identify_search_1", {
    result <- galah_identify("Litoria peronii")
  })
  expect_equal(nrow(result), 1)
})

test_that("galah_identify runs a search on multiple strings", {
  capture_requests("galah_identify_search_2", {
    result <- galah_identify("amphibia", "reptilia", "aves", "mammalia")
  })
  expect_equal(nrow(result), 4)
})

## Doesn't work until search_identifiers() is supported
# test_that("galah_identify works with search = FALSE", {
#   galah_config(run_checks = FALSE)
#   id <- "urn:lsid:biodiversity.org.au:afd.taxon:0490a9ba-0d08-473d-a709-6c42e354f118"
#   result <- galah_identify(id, search = FALSE)
#   expect_equal(result$identifier[1], id)
# })

test_that("galah_identify can pass a string unchanged when run_checks = FALSE", {
  galah_config(run_checks = FALSE)
  result <- galah_identify("a_string", search = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$identifier[1], "a_string")
})

## Adds additional warning for missing taxa that doesn't exist
# test_that("galah_identify pipes correctly", {
#   capture_requests("galah_identify_search_3", {
    # result <- galah_call() |>
    #   galah_identify("Litoria") |>
    #   galah_filter(year == 2020)
#   })
#   expect_false(is.null(result$identify))
#   expect_false(is.null(result$filter)) 
# })
