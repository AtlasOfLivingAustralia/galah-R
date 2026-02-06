quiet_config <- purrr::quietly(galah_config)

x <- quiet_config(atlas = "GBIF",
                  username = "atlasoflivingaustralia",
                  email = "ala4r@ala.org.au",
                  password = "galah-gbif-test-login")

test_that("`galah_filter()` returns predicates for GBIF", {
  x <- galah_filter(year == 2024) 
  inherits(x, "list") |>
    expect_true()
  expect_equal(names(x),
               c("type", "key", "value"))
  expect_equal(x,
               list(type = "equals", 
                    key = "YEAR",
                    value = "2024") |>
                 structure(class = c("predicates_filter", "list")))
})

test_that("`filter()` handles a single entry for GBIF", {
   skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    filter(year == 2024) |>
    count() |>
    collect() 
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)
  expect_true(is.integer(x$count))
})

# FIXME: `check_fields()` not tested for GBIF - try sending invalid fields to `filter()`

test_that("`filter()` handles multiple (`AND`) queries for GBIF", {
  skip_if_offline(); skip_on_ci()
  # get a count limited by two different categories
  x <- galah_call() |>
    filter(year == 2024, basisOfRecord == "HUMAN_OBSERVATION") |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)

  # group by the first category
  y <- galah_call() |>
    filter(year == 2024, basisOfRecord == "HUMAN_OBSERVATION") |>
    group_by(basisOfRecord) |>
    count() |>
    collect()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(y), 1)
  expect_equal(ncol(y), 2)
  # verify that the inputs are shown in the response
  expect_equal(colnames(y)[[1]], "basisOfRecord")
  expect_equal(y$basisOfRecord, "HUMAN_OBSERVATION")

  z <- galah_call() |>
    filter(year == 2024, basisOfRecord == "HUMAN_OBSERVATION") |>
    group_by(year) |>
    count() |>
    collect()
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(z), 1)
  expect_equal(ncol(z), 2)
  # verify that the inputs are shown in the response
  expect_equal(colnames(z)[[1]], "year")
  expect_equal(z$year, "2024")

  # test that all queries return the same sum
  expect_equal(x$count, y$count)
  expect_equal(x$count, z$count)
})

test_that("`count()` errors when real but non-indexed fields are requested", {
  skip_if_offline(); skip_on_ci()

  # invalid fields
  galah_call() |>
    filter(something == 9) |>
    count() |>
    collapse() |>
    expect_error(label = "Can't use fields that don't exist")

  # real, but not indexed, group_by statement
  request_data() |>
    filter(class == "Mammalia") |>
    group_by(order) |>
    count() |>
    collapse() |>
    expect_error(label = "Can't use fields that don't exist")
})

test_that("`count()` works with `identify()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  # collapse
  x <- request_data() |>
    identify("Mammalia") |>
    filter(year >= 2020, basisOfRecord == "HUMAN_OBSERVATION") |>
    group_by(classKey) |>
    count() |>
    collect()
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 2)
  expect_equal(x$classKey, "359")
})
# FIXME: fields returned by show_all(fields) are not the same as those accepted by occurrences/search API
# This leads to real field names being passed to this API, but not affecting the result
# accepted fields are here:
# https://techdocs.gbif.org/en/openapi/v1/occurrence#/Searching%20occurrences/searchOccurrence

test_that("`filter()` handles `OR` and `%in%` for GBIF", {
  skip_if_offline(); skip_on_ci()

  # first prove that these facets are collected
  x <- request_data() |>
    filter(basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN") |>
    group_by(basisOfRecord) |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 2)
  expect_contains(x$basisOfRecord,
                  c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))
  
  # then check the sum against a query without `group_by()`
  y <- request_data() |>
   filter(basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN") |>
   count() |>
   collect()
  expect_equal(sum(x$count), y$count)

  # check that %in% gives the same result
  z <- request_data() |>
    filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
    count() |>
    collect()
  expect_equal(y$count, z$count)
})

test_that("`filter()` handles multiple queries including != for GBIF", {
  x <- galah_call() |>
    filter(year == 2024, country != "AU") |>  # FIXME: countryCode fails with cryptic warning
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)

  # check that `!=` definitely reduces the number of records returned
  y <- galah_call() |>
    filter(year == 2024) |>
    count() |>
    collect()
  expect_lt(x$count, y$count)
})

test_that("`filter()` handles `between()` for GBIF", {
  x <- galah_call() |>
    filter(dplyr::between(year, 2010, 2020)) |>
    group_by(year) |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(x), 2)
  expect_gt(nrow(x), 8)
  expect_lt(nrow(x), 12)
  integer_years <- as.integer(x$year)
  all(integer_years >= 2010 & integer_years <= 2020) |>
    expect_true()
})

test_that("filter() handles !() for GBIF", {

  # exclude some levels of basisOfRecord
  excluded_categories <- c("OCCURRENCE", "LIVING_SPECIMEN", "HUMAN_OBSERVATION")
  x <- galah_call() |>
    filter(!(basisOfRecord %in% excluded_categories)) |>
    group_by(basisOfRecord) |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(x), 2)
  expect_gt(nrow(x), 3)

  # get all levels of basisOfRecord
  y <- galah_call() |>
    group_by(basisOfRecord) |>
    count() |>
    collect()

  # check that those categories - and only those categories - are missing
  missing_categories <- y$basisOfRecord[!(y$basisOfRecord %in% x$basisOfRecord)]
  expect_equal(sort(excluded_categories), 
               sort(missing_categories))
})

test_that("filter() handles `is.na()` for GBIF", {
  # missing values
  x <- galah_call() |>
    filter(is.na(country)) |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)

  # present values
  y <- galah_call() |>
    filter(!is.na(country)) |>
    count() |>
    collect()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(y), 1)
  expect_equal(ncol(y), 1)

  # all values
  z <- galah_call() |>
    count() |>
    collect()
  expect_equal(x$count + y$count, z$count)
})

test_that("filter() handles c() for GBIF", {
  # effectively parses this as 'in' as per GBIF instructions
  country_vector <- c("AU", "US", "NL")

  # check when supplied directly
  x <- galah_call() |>
    filter(country %in% c("AU", "US", "NL")) |>
    group_by(country) |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3)
  expect_equal(ncol(x), 2)
  expect_contains(x$country, country_vector)

  # and as a vector
  y <- galah_call() |>
    filter(country == country_vector) |>
    group_by(country) |>
    count() |>
    collect()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(y), 3)
  expect_equal(ncol(y), 2)
  expect_contains(y$country, country_vector)

  # direct comparison
  expect_identical(x, y)
})

test_that("`count()` works with `galah_polygon()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  # errors when points given clockwise
  wkt <- "POLYGON((142.36 -29.01,142.74 -29.01,142.74 -29.39,142.36 -29.39,142.36 -29.01))"
  expect_error({galah_call() |>
      galah_polygon(wkt) |>
      count() |>
      collect()})
  # works when points given counter-clockwise
  wkt <- "POLYGON((142.36 -29.01,142.36 -29.39,142.74 -29.39,142.74 -29.01,142.36 -29.01))"
  result <- galah_call() |>
    identify("Mammalia") |>
    galah_polygon(wkt) |>
    count() |>
    collect()
  # compare against a taxonomic query in the same place
  result_taxa <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect()
  # compare against a purely spatial query
  result_space <- galah_call() |>
    galah_polygon(wkt) |>
    count() |>
    collect()
  expect_lt(result$count, result_taxa$count)
  expect_lt(result$count, result_space$count)
})

test_that("`count()` works with `galah_radius()` for GBIF", {
  skip_if_offline(); skip_on_ci()
  # ditto for a point and radius
  result <- galah_call() |>
    identify("Mammalia") |>
    galah_radius(lat = -33.7,
                 lon = 151.3,
                 radius = 5) |>
    count() |>
    collect()
  result_space <- galah_call() |>
    galah_radius(lat = -33.7,
                 lon = 151.3,
                 radius = 5) |>
    count() |>
    collect()
  result_taxa <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect()
  expect_lt(result$count, result_taxa$count)
  expect_lt(result$count, result_space$count)
})

# TODO: add assertions?

x <- quiet_config(atlas = "ALA")
rm(x, quiet_config)
