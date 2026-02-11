test_that("`galah_call()` builds objects of class 'data_request' by default", {
  expect_equal(length(galah_call()), 1)
  expect_s3_class(galah_call(), "data_request")
})

test_that("`request_` functions build correct object classes", {
  x <- request_metadata()
  expect_s3_class(x, "metadata_request")
  expect_true(x$type == "fields") 
  y <- request_files()
  expect_s3_class(y, "files_request")
  expect_true(y$type == "media")
})

test_that("`galah_call()` works with all `galah_` functions", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |> 
    identify("Litoria") |>
    filter(year == 2021, cl22 == "Tasmania") |>
    select(year) |>
    apply_profile(ALA) |>
    geolocate("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    group_by(year, basisOfRecord) |>
    arrange(basisOfRecord)
  # ensure no null values
  purrr::map(result, is.null) |>
    unlist() |>
    any() |>
    expect_false()
  # ensure content is added in same order as supplied
  expect_equal(
    names(result),
    c("type", "identify", "filter", "select", "apply_profile",
      "geolocate", "group_by", "arrange"))
})

test_that("`galah_call()` works irrespective of `galah_` function order", {
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
  
test_that("repeated calls to `identify()` are added correctly", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |> 
    identify("Litoria") |>
    identify("Aves")
  expect_equal(nrow(result$identify), 2)
})

test_that("repeated calls to `filter()` are added correctly", { 
  result <- galah_call() |> 
    filter(year >= 2010) |>
    filter(basisOfRecord == "human_observation", cl22 == "Tasmania")
  expect_equal(nrow(result$filter), 3)
})
