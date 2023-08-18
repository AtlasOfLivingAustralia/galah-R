galah_config(verbose = FALSE)

test_that("`collapse()` doesn't ping an API for type = `'occurrences-count'`", {
  result <- request_data(type = "occurrences-count") |>
    filter(year == 2010) |>
    collapse()
  expect_true(inherits(result, "data_query"))
  expect_equal(names(result),
               c("type", "url", "slot_name", "expand", "headers"))
})

with_mock_dir("atlas_counts", {
  test_that("atlas_counts works with no arguments", {
    count <- atlas_counts()
    expect_gt(count$count, 0)
  })
})

test_that("count() |> collect() works with no arguments", {
  skip_if_offline()
  count <- galah_call() |>
    count() |>
    collect()
  expect_gt(count$count, 0)
})

test_that("`identify()` reduces the number of records returned by `count()`", {
  skip_if_offline()
  counts_mammals <- galah_call() |>
    identify("Mammalia") |>
    count() |>
    collect()
  counts_all <- galah_call() |>
    count() |>
    collect()
  expect_type(counts_mammals$count, "integer")
  expect_true(counts_mammals$count < counts_all$count)
})

test_that("`count()` handles multiple 'group by' variables", {
  skip_if_offline()
  counts <- galah_call() |>
    filter(year >= 2021) |>
    group_by(year, basisOfRecord) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(names(counts) %in% c("year", "basisOfRecord", "count")))
})
# })

with_mock_dir("atlas_counts_group_by", {
  test_that("`count()` handles 'species' as a 'group by' variable", {
    counts <- galah_call() |>
      identify("perameles") |>
      filter(year > 2020) |>
      group_by(species, year) |>
      count() |>
      collect()
    expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
    expect_true(all(names(counts) %in% c("year", "species", "count")))
  })
})

test_that("atlas_counts handles 'taxonConceptID' as a 'group by' variable", {
  skip_if_offline()
  counts <- galah_call() |>
    identify("perameles") |>
    filter(year >= 2010) |>
    group_by(taxonConceptID, year) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(names(counts) %in% c("year", "taxonConceptID", "count")))
})

test_that("atlas_counts returns same result with filter using `,` and `&`", {
  skip_if_offline()
  count_comma <- galah_call() |>
    galah_filter(year >= 2010, year < 2020) |>
    count() |>
    collect()
  count_and <- galah_call() |>
    galah_filter(year >= 2010 & year < 2020) |>
    count() |>
    collect()
  expect_equal(count_comma, count_and)
})

# Spatial not checked
test_that("atlas_counts filters correctly with galah_geolocate/galah_polygon", {
  skip_if_offline()
  wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
    sf::st_as_sfc()
  base_query <- galah_call() |>
    identify("dasyurus") |>
    filter(year >= 2020)
  counts <- base_query |>
    count() |>
    collect()
  counts_filtered <- base_query |>
    galah_geolocate(wkt) |>
    count()|>
    collect()
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  expect_lt(count_1, count_2)
})

test_that("atlas_counts filters correctly with galah_geolocate/galah_bbox", {
  skip_if_offline()
  wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
    sf::st_as_sfc()
  base_query <- galah_call() |>
    identify("dasyurus") |>
    filter(year >= 2020)
  counts <- base_query |>
    count() |>
    collect()
  counts_filtered <- base_query |>
    galah_geolocate(wkt, type = "bbox") |>
    count()|>
    collect()
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  expect_lt(count_1, count_2)
})

## BELOW HERE TESTS WILL FAIL

## counts for `type = "species"` not checked
# test_that("atlas_counts returns species counts", {
#   vcr::use_cassette("count_type_species", {
#     counts <- galah_call(type = "species") |> count()
#   })
#   expect_type(counts$count, "integer")
#   expect_gt(counts, 0)
# })

# capture_requests("count_piped_2", {
#   test_that("atlas_counts ignores superfluous piped arguments", {
# counts <- galah_call() |>
#   filter(year >= 2018) |>
#   group_by(year) |>
#   galah_down_to(species) |>
#   select(taxonConceptID) |>
#   count()
#     expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
#     expect_equal(names(counts), c("year", "count"))
#     expect_gt(nrow(counts), 0)
#   })
# })

# test_that("atlas_counts handles pagination", {
#   vcr::use_cassette("count_with_pagination", {
#     counts <- galah_call() |>
#       group_by(year) |>
#       slice_head(n = 101) |>
#       count()
#   })
#   expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
#   expect_equal(nrow(counts), 101)
#   expect_equal(names(counts), c("year", "count"))
# })

## `galah_down_to()` not checked

## slice_head() not checked

## arrange() and arrange(desc()) not checked
