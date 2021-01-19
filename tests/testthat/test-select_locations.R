context("Test select_locations")

test_that("select_locations checks inputs", {
  skip_on_cran()
  poly_path <- "../test_data/act_state_polygon_shp/ACT_STATE_POLYGON_shp.shp"
  expect_error(
    select_locations(sf = st_read(poly_path),
                 wkt = readLines("../testdata/short_act_wkt.txt")))
  expect_error(select_locations(sf =
                                 st_read(poly_path)))
  
  expect_error(select_locations(wkt = readLines("../testdata/long_act_wkt.txt")))
  
})

test_that("select_locations finds polygon errors", {
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_warning(select_locations(wkt = invalid_wkt))
  
  invalid_wkt <- "POLYGON((132.8 -12.72, 132.95 -12.70, 132.92 -12.57, 132.85 -12.58))"
  expect_warning(select_locations(wkt = invalid_wkt))
})
