quiet_config <- purrr::quietly(galah_config)

gbif_config <- quiet_config(atlas = "GBIF",
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
  skip_galah()
  x <- galah_call() |>
    filter(year == 2024) |>
    count() |>
    collect() |>
    skip_unavailable() 
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)
  expect_true(is.integer(x$count))
})

test_that("`filter()` handles multiple (`AND`) queries for GBIF", {
  skip_galah()
  # get a count limited by two different categories
  x <- galah_call() |>
    filter(year == 2024, basisOfRecord == "HUMAN_OBSERVATION") |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)

  # group by the first category
  y <- galah_call() |>
    filter(year == 2024, basisOfRecord == "HUMAN_OBSERVATION") |>
    group_by(basisOfRecord) |>
    count() |>
    collect() |>
    skip_unavailable()
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
    collect() |>
    skip_unavailable()
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

test_that("`filter()` handles `OR` and `%in%` for GBIF", {
  skip_galah()

  # first prove that these facets are collected
  x <- request_data() |>
    filter(basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN") |>
    group_by(basisOfRecord) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 2)
  expect_equal(ncol(x), 2)
  expect_contains(x$basisOfRecord,
                  c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN"))
  
  # then check the sum against a query without `group_by()`
  y <- request_data() |>
    filter(basisOfRecord == "HUMAN_OBSERVATION" | basisOfRecord == "PRESERVED_SPECIMEN") |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lte((sum(x$count) - y$count) / y$count, 0.01)

  # check that %in% gives the same result
  z <- request_data() |>
    filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) |>
    count() |>
    collect() |>
    skip_unavailable()

  # check difference is <1%
  # sometimes get slight bugs with code like this due to server-side estimation error
  expect_lte(abs(y$count - z$count) / y$count, 0.01)
})

test_that("`filter()` handles multiple queries including != for GBIF", {
  skip_galah()

  x <- galah_call() |>
    filter(year == 2024, country != "AU") |>  # FIXME: countryCode fails with cryptic warning
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)

  # check that `!=` definitely reduces the number of records returned
  y <- galah_call() |>
    filter(year == 2024) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_lt(x$count, y$count)
})

test_that("`filter()` handles `between()` for GBIF", {
  skip_galah()

  x <- galah_call() |>
    filter(dplyr::between(year, 2010, 2020)) |>
    group_by(year) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(x), 2)
  expect_gt(nrow(x), 8)
  expect_lt(nrow(x), 12)
  integer_years <- as.integer(x$year)
  all(integer_years >= 2010 & integer_years <= 2020) |>
    expect_true()
})

test_that("`filter()` handles `!()` for GBIF", {
  skip_galah()

  # exclude some levels of basisOfRecord
  excluded_categories <- c("OCCURRENCE", "LIVING_SPECIMEN", "HUMAN_OBSERVATION")
  x <- galah_call() |>
    filter(!(basisOfRecord %in% excluded_categories)) |>
    group_by(basisOfRecord) |>
    count() |>
    collect() |>
    skip_unavailable()
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

test_that("`filter()` handles `is.na()` for GBIF", {
  skip_galah()

  # missing values
  x <- galah_call() |>
    filter(is.na(country)) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)

  # present values
  y <- galah_call() |>
    filter(!is.na(country)) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(y), 1)
  expect_equal(ncol(y), 1)

  # all values
  z <- galah_call() |>
    count() |>
    collect()

  expect_lte(((x$count + y$count) - z$count) / z$count, 0.01)
})

test_that("`filter()` handles `c()` for GBIF", {
  skip_galah()

  # effectively parses this as 'in' as per GBIF instructions
  country_vector <- c("AU", "US", "NL")

  # check when supplied directly
  x <- galah_call() |>
    filter(country %in% c("AU", "US", "NL")) |>
    group_by(country) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3)
  expect_equal(ncol(x), 2)
  expect_contains(x$country, country_vector)

  # and as a vector
  y <- galah_call() |>
    filter(country == country_vector) |>
    group_by(country) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(y), 3)
  expect_equal(ncol(y), 2)
  expect_contains(y$country, country_vector)

  # direct comparison
  expect_all_true(sort(x$country) == sort(y$country))
  expect_all_true((x$count - y$count) / y$count < 0.01)
})

# TODO: add assertions?

gbif_config <- quiet_config(atlas = "ALA")
rm(gbif_config, quiet_config)
