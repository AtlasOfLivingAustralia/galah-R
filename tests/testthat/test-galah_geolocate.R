context("Test galah_geolocate")

test_that("galah_geolocate checks inputs", {
  skip_on_cran()
  poly_path <- "../testdata/act_state_polygon_shp/ACT_STATE_POLYGON_shp.shp"
  expect_error(
    galah_geolocate(
      st_read(poly_path, quiet = TRUE),
      readLines("../testdata/short_act_wkt.txt")))
  expect_error(galah_geolocate(st_read(poly_path, quiet = TRUE)))
  expect_error(galah_geolocate(readLines("../testdata/long_act_wkt.txt")))
})

test_that("galah_geolocate finds polygon errors", {
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_warning(galah_geolocate(invalid_wkt))

  invalid_wkt <- "POLYGON((132.8 -12.72, 132.95 -12.70, 132.92 -12.57, 132.85 -12.58))"
  expect_warning(galah_geolocate(invalid_wkt))
})

test_that("galah_geolocate converts to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_match(galah_geolocate(wkt), "MULTIPOLYGON")
})

test_that("galah_geolocate converts sf object to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_match(build_wkt(st_as_sfc(wkt)), "MULTIPOLYGON")
})
