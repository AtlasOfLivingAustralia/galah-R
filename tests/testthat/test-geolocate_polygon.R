test_that("`geolocate_polygon()` uses first argument", {
  wkt_1 <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  wkt_2 <- "POLYGON((145.6765 -42.13203, 145.9652 -42.63203, 146.5425 -42.63203, 146.8312 -42.13203, 146.5425 -41.63203, 145.9652 -41.63203, 145.6765 -42.13203))"
  expected_polygon <- "MULTIPOLYGON (((142.3623 -29.00703, 142.7413 -29.00703, 142.7413 -29.39064, 142.3623 -29.39064, 142.3623 -29.00703)))"
  warning_text <- expect_warning(geolocate_polygon(wkt_1, wkt_2))
  grepl("More than 1 spatial area provided", warning_text) |>
    any() |>
    expect_true()
  quiet_polygons <- purrr::quietly(geolocate_polygon)
  calculated_polygon <- quiet_polygons(wkt_1, wkt_2)$result
  expect_identical(calculated_polygon, 
                   geolocate_polygon(wkt_1)[1],
                   expected_polygon)
})

test_that("`geolocate_polygon()` checks inputs", {
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  sf::st_read(poly_path, quiet = TRUE) |>
    geolocate_polygon() |>
    expect_error()

  wkt_path <- test_path("testdata", "long_act_wkt.txt")
  readLines(wkt_path) |>
    geolocate_polygon() |>
    expect_error()
})

test_that("`geolocate_polygon()` finds polygon errors 1" , {
  skip_if_offline(); skip_on_ci()
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  geolocate_polygon(invalid_wkt) |>
    expect_error()
  # NOTE: 
    # this code generates an extra string ('OGR: Corrupt data')
    # that is useful, but can't be stopped by tryCatch. Skipping on CRAN
    # to avoid unnecessary error messages
})

test_that("`geolocate_polygon()` finds polygon errors 2", {
  invalid_wkt <- "POLYGON((132.8 -12.72, 132.95 -12.70, 132.92 -12.57, 132.85 -12.58))"
  geolocate_polygon(invalid_wkt) |>
    expect_error()
})

test_that("`geolocate_polygon()` converts WKT strings to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  geolocate_polygon(wkt) |>
    expect_match("MULTIPOLYGON")
})

test_that("`geolocate_polygon()` converts WKT strings with spaces", {
  wkt_with_spaces <- "POLYGON ((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  converted_wkt_with_spaces <- wkt_with_spaces |>
    sf::st_as_sfc() |>
    build_wkt()
  expect_match(converted_wkt_with_spaces, "MULTIPOLYGON \\(\\(\\(143\\.32")
})

test_that("`geolocate_polygon()` converts sf object to multipolygon", {
  sf_wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))" |> 
    sf::st_as_sfc()
  geolocate_polygon(sf_wkt) |>
    expect_match("MULTIPOLYGON")
})

test_that("`geolocate_polygon()` counts vertices correctly", {
  sf::st_as_sfc("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))") |>
    n_points() |>
   expect_equal(4)
})

test_that("`geolocate_polygon()` checks for simple polygons only", {
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  shapefile_complex <- sf::st_read(poly_path, quiet = TRUE)
  shapefile_simple <- sf::st_simplify(shapefile_complex, dTolerance = 1000)
  geolocate_polygon(shapefile_complex) |>
    expect_error(label = "Polygon must have 500 or fewer vertices")
  geolocate_polygon(shapefile_simple) |>
    expect_match(regexp = "MULTIPOLYGON")
})

test_that("`geolocate_polygon()` counts n vertices correctly", {
  sf_wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))" |> 
    sf::st_as_sfc()
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  shapefile_complex <- sf::st_read(poly_path, quiet = TRUE)
  expect_equal(n_points(shapefile_complex), 2787)
  expect_equal(n_points(sf_wkt), 4)
})

test_that("`geolocate_polygon()` warns when CRS isn't EPSG:4326", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf_gda94 <- sf::st_as_sfc(wkt, crs = sf::st_crs(4283)) # wrong GDA94
  expect_warning(geolocate(obj_sf_gda94), "Spatial object CRS")
})


# Future: test that geolocate_polygon accepts nothing as an input and ignores