context("Test masked functions")

test_that("`identify` works identically to piped `galah_identify`", {
  vcr::use_cassette("identify_test", {
    result1 <- galah_call() |> galah_identify("Litoria")
    result2 <- galah_call() |> identify("Litoria")
  })
  expect_equal(result1, result2)
})

test_that("`filter` works identically to piped `galah_filter`", {
  result1 <- galah_call() |> filter(year == 2010)
  result2 <- galah_call() |> galah_filter(year == 2010)
  expect_equal(result1, result2)
})

test_that("`select` works identically to piped `galah_select`", {
  result1 <- galah_call() |> select(year, group = "basic")
  result2 <- galah_call() |> galah_select(year, group = "basic")
  expect_equal(result1, result2)
})

test_that("`group_by` works identically to piped `galah_group_by`", {
  result1 <- galah_call() |> group_by(year, basisOfRecord)
  result2 <- galah_call() |> galah_group_by(year, basisOfRecord)
  expect_equal(result1, result2)
})

test_that("`st_crop` works identically to piped `galah_polygon`", {
  location <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  result1 <- galah_call() |> st_crop(location)
  result2 <- galah_call() |> galah_polygon(location)
  expect_equal(result1, result2)
})

test_that("`count` works identically to piped `atlas_counts`", {
  vcr::use_cassette("count_test", {
    result1 <- galah_call() |> galah_identify("Litoria") |> atlas_counts()
    result2 <- galah_call() |> identify("Litoria") |> count()
  })
  expect_equal(result1, result2)
})

test_that("`slice_head` works for atlas_counts", {
  vcr::use_cassette("slice_head", {
    result <- galah_call() |> 
      filter(year >= 2010) |> 
      group_by(year) |> 
      slice_head(n = 5) |>
      count()
  })
  expect_equal(nrow(result), 5)
})