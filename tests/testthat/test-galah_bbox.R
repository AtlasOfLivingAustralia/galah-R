test_that("galah_bbox returns bbox for sf", {
  polygon_sfc <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))" |>
    st_as_sfc()
  polygon_bbox <- galah_bbox(polygon_sfc)
  expected_bbox <- polygon_sfc |> st_bbox()
  expect_message(galah_bbox(polygon_sfc), "Data returned for bounding box:")
  expect_true(grepl("MULTIPOLYGON", galah_bbox(polygon_sfc)))
  expect_equal(attributes(polygon_bbox)$bbox, expected_bbox)
})

test_that("galah_bbox returns bbox for shapefile", {
  poly_path <- test_path("testdata", "act_state_polygon_shp", "ACT_STATE_POLYGON_shp.shp")
  shapefile <- sf::st_read(poly_path, quiet = TRUE)
  shapefile_bbox <- galah_bbox(shapefile)
  expected_bbox <- attributes(shapefile_bbox)$bbox
  expect_message(galah_bbox(shapefile), "Data returned for bounding box:")
  expect_true(grepl("MULTIPOLYGON", galah_bbox(shapefile)))
  expect_equal(attributes(shapefile_bbox)$bbox, expected_bbox)
})

test_that("galah_bbox returns bbox for bbox", { # FIXME: not backwards compatible with bbox coords?
  bbox <- st_bbox(c(xmin = 143, xmax = 148, ymin = -29, ymax = -28), crs = st_crs("WGS84"))
  bbox_galah <- galah_bbox(bbox)
  expected_polygon <- "MULTIPOLYGON (((143 -29, 148 -29, 148 -28, 143 -28, 143 -29)))"
  expect_message(galah_bbox(bbox), "Data returned for bounding box:")
  expect_equal(galah_bbox(bbox)[1], expected_polygon)
  expect_equal(attributes(bbox_galah)$bbox, bbox)
})

test_that("galah_bbox returns bbox for tibble", {
  tibble <- tibble(xmin = 143, ymin = -29, xmax = 148, ymax = -21)
  tibble_bbox <- galah_bbox(tibble)
  expected_polygon <- "MULTIPOLYGON (((143 -29, 148 -29, 148 -21, 143 -21, 143 -29)))"
  expected_bbox <- st_bbox(c(xmin = 143, xmax = 148, ymin = -29, ymax = -21), crs = st_crs("WGS84"))
  expect_message(galah_bbox(tibble), "Data returned for bounding box:")
  expect_equal(galah_bbox(tibble)[1], expected_polygon)
  expect_equal(attributes(tibble_bbox)$bbox, expected_bbox)
})

test_that("galah_bbox does not accept incorrect tibbles", {
  tibble_wrong <- tibble(c1 = c("hi", "hello"), c2 = 1:2)
  tibble_bad_colnames <- tibble(top = 148, bottom = -29, ymin = -29, ymax = -29)
  tibble_invalid_bbox <- tibble(xmin = 148, ymin = -29, xmax = 143, ymax = -29)
  expect_error(galah_bbox(tibble_wrong))
  expect_error(galah_bbox(tibble_bad_colnames))
  expect_error(galah_bbox(tibble_invalid_bbox))
})

test_that("galah_bbox uses only first coordinates of tibble with many coordinates", {
  tibble_many_coords <- tibble(xmin = c(148, 145), 
                               ymin = c(-29, -42), 
                               xmax = c(143, 146), 
                               ymax = c(-30, -41))
  expect_warning(galah_bbox(tibble_many_coords), 
                 "More than 1 set of coordinates supplied to")
})

test_that("galah_bbox checks number of inputs, uses first argument", { # FIXME
  wkt_1 <- glue("POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 \\
                -29.39064,142.36228 -29.39064,142.36228 -29.00703))") |> st_as_sfc()
  wkt_2 <- glue("POLYGON((145.6765 -42.13203, 145.9652 -42.63203, 146.5425 \\
                -42.63203, 146.8312 -42.13203, 146.5425 -41.63203, 145.9652 \\
                -41.63203, 145.6765 -42.13203))") |> st_as_sfc()
  expected_polygon <- glue("MULTIPOLYGON (((142.3623 -29.39064, 142.7413 -29.39064, \\
                           142.7413 -29.00703, 142.3623 -29.00703, 142.3623 -29.39064)))")
  bbox_1 <- expect_warning(galah_bbox(wkt_1, wkt_2), "More than 1 spatial area provided")
  expect_equal(as.character(bbox_1), 
               as.character(galah_bbox(wkt_1)[1]), 
               as.character(expected_polygon))
})

test_that("galah_bbox checks inputs", {
  wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
  number <- 45
  c_char <- c("a", "b", "c", "d")
  c_numbers <- c(45, 2, 45, 2)
  expect_error(galah_bbox(wkt))
  expect_error(galah_bbox(number))
  expect_error(galah_bbox(c_char))
  expect_error(galah_bbox(c_numbers))
})

test_that("galah_bbox detects invalid spatial objects", {
  impossible_bbox <- st_bbox(c(xmin = 148000, 
                               xmax = -29000, 
                               ymin = -29000, 
                               ymax = -29000),
                             crs = st_crs("WGS84"))
  expect_error(galah_bbox(impossible_bbox), 
               "Invalid spatial object")
})

test_that("galah_bbox converts to multipolygon", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  sf_wkt <- wkt |> st_as_sfc()
  expect_match(galah_bbox(sf_wkt), "MULTIPOLYGON")
})

# TODO: test that galah_bbox unnests sf object & shapefiles correctly 
# after converting from dots