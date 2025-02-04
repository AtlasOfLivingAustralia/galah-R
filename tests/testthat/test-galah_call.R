## Note: "order" arg was removed from galah_call, adding it back will require updating of these tests

test_that("galah_call builds objects of class 'data_request' by default", {
  expect_equal(length(galah_call()), 8)
  expect_s3_class(galah_call(), "data_request")
})

test_that("galah_call accepts method arg", {
  x <- galah_call(method = "metadata")
  expect_s3_class(x, "metadata_request")
  expect_true(x$type == "fields") 
  y <- galah_call(method = "files")
  expect_s3_class(y, "files_request")
  expect_true(y$type == "media")
  expect_error(galah_call(method = "nothing"))
})

# prints properly?

test_that("galah_call works with all `galah_` functions", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |> 
    galah_identify("Litoria") |>
    galah_filter(year == 2021, cl22 == "Tasmania") |>
    galah_select(year) |>
    galah_apply_profile(ALA) |>
    galah_geolocate("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    galah_group_by(year, basisOfRecord) |>
    arrange(basisOfRecord)
  expect_false(any(unlist(lapply(result, is.null))))   
})

test_that("galah_call works irrespective of `galah_` function order", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |> 
    galah_apply_profile(ALA) |>
    galah_group_by(year, basisOfRecord) |>
    arrange(basisOfRecord) |>
    galah_geolocate("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    galah_select(year) |>
    galah_filter(year == 2021, cl22 == "Tasmania") |>
    galah_identify("Litoria")
  expect_false(any(unlist(lapply(result, is.null))))   
})
  
test_that("repeated calls to `galah_identify` are added correctly", {
  skip_if_offline(); skip_on_ci()
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
