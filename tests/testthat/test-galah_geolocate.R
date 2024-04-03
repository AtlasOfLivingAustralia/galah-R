test_that("galah_geolocate defaults to galah_polygon", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  expect_match(galah_geolocate(wkt), "MULTIPOLYGON")
})

test_that("galah_geolocate works when type is set to polygon", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  expect_match(galah_geolocate(wkt, type = "polygon"), "MULTIPOLYGON")
})

test_that("galah_geolocate switches to use galah_bbox when `type = bbox`", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf <- wkt |> sf::st_as_sfc()
  expect_error(galah_geolocate(wkt, type = "bbox"))
  expect_match(galah_geolocate(obj_sf, type = "bbox"), "MULTIPOLYGON")
})

test_that("galah_geolocate switches to use galah_radius when `type = radius`", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf <- wkt |> sf::st_as_sfc()
  expect_error(galah_geolocate(wkt, type = "radius"), "Missing")
  expect_error(galah_geolocate(obj_sf, type = "radius"), "Invalid spatial object")
  expect_equal(galah_geolocate(lat = -33.66741,
                               lon = 151.3174,
                               radius = 2, 
                               type = "radius"), 
               list(lat = -33.66741,
                    lon = 151.3174,
                    radius = 2))
})
