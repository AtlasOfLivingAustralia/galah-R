test_that("galah_polygon uses first argument", {
  wkt_1 <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  wkt_2 <- "POLYGON((145.6765 -42.13203, 145.9652 -42.63203, 146.5425 -42.63203, 146.8312 -42.13203, 146.5425 -41.63203, 145.9652 -41.63203, 145.6765 -42.13203))"
  expected_polygon <- "MULTIPOLYGON (((142.3623 -29.00703, 142.7413 -29.00703, 142.7413 -29.39064, 142.3623 -29.39064, 142.3623 -29.00703)))"
  polygon_1 <- expect_warning(galah_polygon(wkt_1, wkt_2))
  expect_equal(as.character(polygon_1), 
               galah_polygon(wkt_1)[1], 
               expected_polygon)
})

test_that("galah_polygon checks inputs", {
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  wkt_path <- test_path("testdata", "long_act_wkt.txt")
  expect_error(galah_polygon(st_read(poly_path, quiet = TRUE)))
  expect_error(galah_polygon(readLines(wkt_path)))
})

test_that("galah_polygon finds polygon errors 1" , {
  skip_on_cran()
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_error(galah_polygon(invalid_wkt))
  # NOTE: 
    # this code generates an extra string ('OGR: Corrupt data')
    # that is useful, but can't be stopped by tryCatch. Skipping on CRAN
    # to avoid unnecessary error messages
})

test_that("galah_polygon finds polygon errors 2", {
  invalid_wkt <- "POLYGON((132.8 -12.72, 132.95 -12.70, 132.92 -12.57, 132.85 -12.58))"
  expect_error(galah_polygon(invalid_wkt))
})

test_that("galah_polygon converts WKT strings to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_match(galah_polygon(wkt), "MULTIPOLYGON")
})

test_that("galah_polygon converts WKT strings with spaces", {
  wkt_with_spaces <- "POLYGON ((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  converted_wkt_with_spaces <- build_wkt(st_as_sfc(wkt_with_spaces))
  expect_match(converted_wkt_with_spaces, "MULTIPOLYGON \\(\\(\\(143\\.32")
})

test_that("galah_polygon converts sf object to multipolygon", {
  sf_wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))" |> st_as_sfc()
  expect_match(galah_polygon(sf_wkt), "MULTIPOLYGON")
})

test_that("galah_polygon counts vertices correctly", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_equal(n_points(st_as_sfc(wkt)), 4)
})

test_that("galah_polygon checks for simple polygons only", {
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  shapefile_complex <- sf::st_read(poly_path, quiet = TRUE)
  shapefile_simple <- sf::st_simplify(shapefile_complex, dTolerance = 1000)
  expect_error(galah_polygon(shapefile_complex), "Polygon must have 500 or fewer vertices")
  expect_match(galah_polygon(shapefile_simple), "MULTIPOLYGON")
})

test_that("galah_polygon counts n vertices correctly", {
  sf_wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))" |> st_as_sfc()
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  shapefile_complex <- sf::st_read(poly_path, quiet = TRUE)
  expect_equal(n_points(shapefile_complex), 2787)
  expect_equal(n_points(sf_wkt), 4)
})


# Future: test that galah_polygon accepts nothing as an input and ignores