context("Test ala geometry")

test_that("ala geometry checks inputs", {
  skip_on_cran()
  poly_path <- "../test_data/act_state_polygon_shp/ACT_STATE_POLYGON_shp.shp"
  expect_error(
    ala_geometry(area = st_read(poly_path),
                wkt = readLines("../test_data/short_act_wkt.txt")))
  expect_error(ala_geometry(area =
                              st_read(poly_path)))
  
  expect_error(ala_geometry(wkt = readLines("../testdata/long_act_wkt.txt")))
  
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_error(ala_geometry(wkt = invalid_wkt))
})

