test_that("`galah_call()` builds objects of class 'data_request' by default", {
  expect_equal(length(galah_call()), 2)
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

test_that("request_data(from = X) sets `atlas` slot in later objects", {
  query_initial <- request_data(from = "GBIF") |> 
    filter(year == 2010) |>
    count()
  expect_equal(query_initial$atlas, "Global")
  query_capture <- capture(query_initial)
  expect_equal(query_capture$atlas, "Global")
  query_compound <- compound(query_capture)
  compound_atlases <- purrr::map(query_compound, 
                                \(a){purrr::pluck(a, "atlas")}) |>
    unlist()
  expect_true(all(compound_atlases == "Global"))
  query_collapse <- collapse(query_compound)
  expect_equal(query_collapse$atlas, "Global")
  query_compute <- compute(query_collapse)
  expect_equal(query_compute$atlas, "Global")
  collect(query_compute)
  # add test to ensure that galah_config() hasn't been updated to GBIF
})

test_that("`galah_call()` works with all `dplyr` functions", {
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
    c("type", "atlas", "identify", "filter", "select", "apply_profile",
      "geolocate", "group_by", "arrange"))
})

test_that("`galah_call()` works irrespective of `galah_` function order", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |> 
    apply_profile(ALA) |>
    group_by(year, basisOfRecord) |>
    arrange(basisOfRecord) |>
    geolocate("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    select(year) |>
    filter(year == 2021, cl22 == "Tasmania") |>
    identify("Litoria")
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
