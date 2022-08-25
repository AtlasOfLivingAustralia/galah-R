context("Test galah_polygon")

test_that("galah_polygon uses first argument", { # FIXME
  skip_on_cran()
  wkt_1 <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  wkt_2 <- "POLYGON((145.6765 -42.13203, 145.9652 -42.63203, 146.5425 -42.63203, 146.8312 -42.13203, 146.5425 -41.63203, 145.9652 -41.63203, 145.6765 -42.13203))"
  expected_polygon <- "MULTIPOLYGON (((142.3623 -29.00703, 142.7413 -29.00703, 142.7413 -29.39064, 142.3623 -29.39064, 142.3623 -29.00703)))"
  expect_warning(galah_polygon(wkt_1, wkt_2))
  expect_equal(galah_polygon(wkt_1, wkt_2)[1], 
               galah_polygon(wkt_1)[1], 
               expected_polygon)
})

test_that("galah_polygon checks inputs", {
  skip_on_cran()
  poly_path <- "../testdata/act_state_polygon_shp/ACT_STATE_POLYGON_shp.shp"
  expect_error(
    galah_polygon(
      st_read(poly_path, quiet = TRUE),
      readLines("../testdata/short_act_wkt.txt")))
  expect_error(galah_polygon(st_read(poly_path, quiet = TRUE)))
  expect_error(galah_polygon(readLines("../testdata/long_act_wkt.txt")))
})

test_that("galah_polygon finds polygon errors", {
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_error(galah_polygon(invalid_wkt))
  
  invalid_wkt <- "POLYGON((132.8 -12.72, 132.95 -12.70, 132.92 -12.57, 132.85 -12.58))"
  expect_error(galah_polygon(invalid_wkt))
})

test_that("galah_polygon converts to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_match(galah_polygon(wkt), "MULTIPOLYGON")
})

#FIXME: Urgh why does this not work?
# test_that("galah_polygon converts wkt strings with spaces", {
#   wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
#   wkt_with_spaces <- "POLYGON ((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
#   converted_wkt <- galah_polygon(wkt)
#   converted_wkt_with_spaces <- galah_polygon(wkt_with_spaces)
#   expect_match(converted_wkt, converted_wkt_with_spaces)
# })

test_that("galah_polygon converts sf object to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  wkt_with_spaces <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))"
  expect_match(build_wkt(st_as_sfc(wkt)), "MULTIPOLYGON")
  expect_match(build_wkt(st_as_sfc(wkt_with_spaces)), "MULTIPOLYGON")
})

test_that("galah_polygon counts vertices correctly", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_equal(n_points(st_as_sfc(wkt)), 4)
})

# galah_polygon accepts wkt strings with spaces
# galah_polygon accepts sf and sfc objects
# galah_polygon checks that the polygon is simple

# Future: galah_polygon accepts nothing as an input and ignores when it's invalid

