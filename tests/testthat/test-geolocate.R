purrr_geolocate <- purrr::quietly(geolocate)
quiet_geolocate <- function(...){purrr_geolocate(...)$result}

test_that("`geolocate()` defaults to `geolocate_polygon`(", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  expect_match(geolocate(wkt), "MULTIPOLYGON")
})

test_that("`geolocate()` works when type is set to polygon", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  polygon <- quiet_geolocate(wkt, type = "polygon")
  expect_match(polygon, "MULTIPOLYGON")
})

test_that("`geolocate()` switches to use `geolocate_bbox()` when `type = bbox`", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf <- wkt |> sf::st_as_sfc()
  geolocate(wkt, type = "bbox") |>
    expect_error(label = "input must be an sf object, data.frame or tibble")
  polygon <- quiet_geolocate(obj_sf, type = "bbox")
  expect_match(polygon, "MULTIPOLYGON")
})

test_that("geolocate switches to use galah_radius when `type = radius`", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  obj_sf <- wkt |> sf::st_as_sfc()
  geolocate(wkt, type = "radius") |>
    expect_error("Missing")
  geolocate(obj_sf, type = "radius") |>
    expect_error("Invalid spatial object")
  radius_obj <- quiet_geolocate(lat = -33.66741,
                                lon = 151.3174,
                                radius = 2, 
                                type = "radius")
  expect_equal(radius_obj, 
               list(lat = -33.66741,
                    lon = 151.3174,
                    radius = 2))
})

rm(purrr_geolocate, quiet_geolocate)
