context("Test galah_call")

test_that("galah_call builds objects of class 'data_request'", {
  expect_equal(length(galah_call()), 6)
  expect_equal(class(galah_call()), "data_request")
})

# prints properly?

test_that("galah_call works with all `galah_` functions", {
  result <- galah_call() |> 
    galah_identify("Litoria") |>
    galah_filter(year == 2021, cl22 == "Tasmania") |>
    galah_select(year) |>
    galah_geolocate("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    galah_group_by(year, basisOfRecord) |>
    galah_down_to(rank = species)
  expect_false(any(unlist(lapply(result, is.null))))   
})

test_that("galah_call works irrespective of `galah_` function order", {
  result <- galah_call() |> 
    galah_down_to(rank = species) |>
    galah_group_by(year, basisOfRecord) |>
    galah_geolocate("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    galah_select(year) |>
    galah_filter(year == 2021, cl22 == "Tasmania") |>
    galah_identify("Litoria")
  expect_false(any(unlist(lapply(result, is.null))))   
})
  
test_that("repeated calls to `galah_identify` are added correctly", {
  result <- galah_call() |> 
    galah_identify("Litoria") |>
    galah_identify("Aves")
  expect_equal(nrow(result$identify), 2)
})

test_that("repeated calls to `galah_filter` are added correctly", { 
  result <- galah_call() |> 
    galah_filter(year >= 2010) |>
    galah_filter(basisOfRecord == "human_observation", cl22 == "Tasmania")
  expect_equal(nrow(result$filter), 3)
})