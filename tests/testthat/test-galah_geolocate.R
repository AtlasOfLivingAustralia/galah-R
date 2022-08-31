context("Test galah_geolocate")

test_that("galah_geolocate defaults to galah_polygon", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  expect_match(galah_geolocate(wkt), "MULTIPOLYGON")
})

test_that("galah_geolocate works when type is set to polygon", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  expect_match(galah_geolocate(wkt, type = "polygon"), "MULTIPOLYGON")
})

test_that("galah_geolocate switches to use galah_bbox when type is set to bbox", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf <- wkt |> sf::st_as_sfc()
  expect_error(galah_geolocate(wkt, type = "bbox"))
  expect_match(galah_geolocate(obj_sf, type = "bbox"), "MULTIPOLYGON")
})
