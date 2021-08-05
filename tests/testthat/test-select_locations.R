context("Test select_locations")

test_that("select_locations checks inputs", {
  skip_on_cran()
  poly_path <- "../testdata/act_state_polygon_shp/ACT_STATE_POLYGON_shp.shp"
  expect_error(
    select_locations(st_read(poly_path),
                 readLines("../testdata/short_act_wkt.txt")))
  expect_error(select_locations(st_read(poly_path)))
  expect_error(select_locations(readLines("../testdata/long_act_wkt.txt")))
})

test_that("select_locations finds polygon errors", {
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_warning(select_locations(invalid_wkt))

  invalid_wkt <- "POLYGON((132.8 -12.72, 132.95 -12.70, 132.92 -12.57, 132.85 -12.58))"
  expect_warning(select_locations(invalid_wkt))
})

test_that("select_locations converts to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_match(select_locations(wkt), "MULTIPOLYGON")
})

test_that("select_locations converts sf object to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_match(build_wkt(st_as_sfc(wkt)), "MULTIPOLYGON")
})
