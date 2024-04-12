
test_that("galah_radius returns list from lon/lat/radius arguments", {
  lon <- 151.3174
  lat <- -33.66741
  radius = 3
  radius_object <- galah_radius(lon = lon, 
                                lat = lat, 
                                radius = radius)
  expected_object <- list(lat = -33.66741,
                          lon = 151.3174,
                          radius = 3)
  expect_equal(radius_object, expected_object)
})

test_that("galah_radius assigns default radius when missing argument", {
  lon <- 151.3174
  lat <- -33.66741
  radius_object <- expect_warning(galah_radius(lon = lon, lat = lat),
                                  "No radius value specified.")
  expected_object <- list(lat = -33.66741,
                          lon = 151.3174,
                          radius = 10) # default is 10 km
  expect_equal(radius_object, expected_object)
})

test_that("galah_radius returns radius for sf_POINT object", {
  point <- sf::st_sfc(sf::st_point(c(-33.66741, 151.3174)), 
                      crs = 4326)
  expected_object <- list(lat = -33.66741,
                          lon = 151.3174,
                          radius = 3)
  expect_equal(galah_radius(point, radius = 3), expected_object)
})

test_that("galah_radius errors when more complex sf objects are passed", {
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  shapefile <- sf::st_read(poly_path, quiet = TRUE)
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf <- wkt |> sf::st_as_sfc()
  expect_error(galah_radius(shapefile, radius = 3), "Invalid spatial object supplied")
  expect_error(galah_radius(obj_sf, radius = 3), "Invalid spatial object supplied")
})

test_that("galah_radius detects inputs", {
  lon_char <- "wrongo"
  lon_list <- list(lon = 151)
  lon_df <- data.frame(lon = 151)
  lat <- -33.66741
  radius = 3
  expect_error(galah_radius(lon = lon_char, 
                            lat = lat,
                            radius = radius), "Invalid class detected")
  expect_error(galah_radius(lon = lon_list, 
                            lat = lat,
                            radius = radius), "Invalid class detected")
  expect_error(galah_radius(lon = lon_df, 
                            lat = lat,
                            radius = radius), "Invalid class detected")
})

test_that("galah_radius detects impossible coordinates", {
  lon1 <- 182
  lon2 <- -195
  lat1 <- -91
  lat2 = 109
  expect_error(galah_radius(lon = lon1,
                            lat = -32,
                            radius = 2),
               "Point location outside of possible range")
  expect_error(galah_radius(lon = lon2,
                            lat = -32,
                            radius = 2),
               "Point location outside of possible range")
  expect_error(galah_radius(lon = 151,
                            lat = lat1,
                            radius = 2),
               "Point location outside of possible range")
  expect_error(galah_radius(lon = 151,
                            lat = lat2,
                            radius = 2),
               "Point location outside of possible range")
})

test_that("galah_radius messages when radius is very large", {
  radius <- 1600
  expect_message(
    galah_radius(lon = 151, lat = -32, radius = radius),
    "Radius is larger than the area of Australia")
})

test_that("galah_radius only uses first arguments supplied to lon/lat/radius", {
  multiple_lon <- c(151, 152, 153)
  multiple_lat <- c(-31, -32, -33)
  multiple_radius <- c(3, 2, 1)
  radius <- 3
  expected_object <- list(lat = -31,
                          lon = 151,
                          radius = 3)
  expect_warning(galah_radius(lon = multiple_lon,
                              lat = -31,
                              radius = radius),
                 "More than 1 spatial")
  expect_warning(galah_radius(lon = 151,
                              lat = multiple_lat,
                              radius = radius),
                 "More than 1 spatial")
  expect_warning(galah_radius(lon = 151,
                              lat = -31,
                              radius = multiple_radius),
                 "More than 1 radius")
  
  expect_equal(
    suppressWarnings(
      galah_radius(lon = multiple_lon, lat = -31, radius = radius)$lon),
      expected_object$lon
      )
  expect_equal(
    suppressWarnings(
      galah_radius(lon = 151, lat = multiple_lat, radius = radius)$lat),
      expected_object$lat
      )
  expect_equal(
    suppressWarnings(
      galah_radius(lon = 151, lat = -31, radius = multiple_radius)$radius),
      expected_object$radius
      )
})

# TODO: (after implementing) galah_radius uses only first coordinates of tibble with many coordinates
