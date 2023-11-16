galah_config(verbose = FALSE)

test_that("`collapse()` doesn't ping an API for type = `'occurrences-count'`", {
  result <- request_data() |>
    filter(year == 2010) |>
    count() |>
    collapse()
  expect_true(inherits(result, "query_set"))
  types <- unlist(lapply(result, function(a){a$type}))
  expect_equal(types,
               c("metadata/fields", "metadata/assertions", "data/occurrences-count"))
})

test_that("atlas_counts works with no arguments", {
  skip_if_offline()
  count <- atlas_counts()
  expect_s3_class(count, c("tbl_df", "tbl", "data.frame"))
  expect_gt(count$count, 0)
})

test_that("count() |> collect() works with no arguments", {
  skip_if_offline()
  count <- galah_call() |>
    count() |>
    collect()
  expect_s3_class(count, c("tbl_df", "tbl", "data.frame"))
  expect_gt(count$count, 0)
})

test_that("`identify()` reduces the number of records returned by `count()`", {
  skip_if_offline()
  counts_all <- galah_call() |>
    count() |>
    collect()
  counts_mammals <- galah_call() |>
    identify("Perameles") |>
    count() |>
    collect()
  expect_type(counts_mammals$count, "integer")
  expect_true(counts_mammals$count < counts_all$count)
})

test_that("`galah_identify()` works with `atlas_counts()`", {
  skip_if_offline()
  counts_all <- galah_call() |>
    count() |>
    collect()
  counts_mammals <- galah_call() |>
    galah_identify("Perameles") |>
    atlas_counts()
  expect_type(counts_mammals$count, "integer")
  expect_true(counts_mammals$count < counts_all$count)
})

test_that("`count()` handles multiple 'group by' variables", {
  skip_if_offline()
  counts <- galah_call() |>
    filter(year >= 2021) |>
    group_by(year, month, basisOfRecord) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts),
               c("year", "month", "basisOfRecord", "count"))
  expect_true(all(counts$year >= 2021))
})

test_that("`count()` handles 'species' as a 'group by' variable", {
  skip_if_offline()
  counts <- galah_call() |>
    filter(year > 2020) |>
    identify("Perameles") |>
    group_by(species, year) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(names(counts) %in% c("species", "year", "count")))
  expect_true(all(counts$year > 2020))
  expect_true(all(grepl("^Perameles", counts$species)))
})

test_that("atlas_counts handles 'taxonConceptID' as a 'group by' variable", {
  skip_if_offline()
  counts <- galah_call() |>
    identify("Perameles") |>
    filter(year >= 2015) |>
    group_by(taxonConceptID, year) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts),
               c("taxonConceptID", "year", "count"))
  expect_true(all(counts$year >= 2015))
})

test_that("atlas_counts returns same result with filter using `,` and `&`", {
  skip_if_offline()
  count_comma <- galah_call() |>
    filter(year >= 2010, year < 2020) |>
    count() |>
    collect()
  count_and <- galah_call() |>
    filter(year >= 2010 & year < 2020) |>
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
    identify("Perameles") |>
    filter(year >= 2020) |>
    count()
  counts <- base_query |> collect()
  counts_filtered <- base_query |>
    galah_geolocate(wkt) |>
    collect()
  expect_s3_class(counts_filtered, c("tbl_df", "tbl", "data.frame"))
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  expect_lt(count_1, count_2)
})

test_that("atlas_counts filters correctly with galah_geolocate/galah_bbox", {
  skip_if_offline()
  wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
    sf::st_as_sfc()
  base_query <- galah_call() |>
    identify("Perameles") |>
    filter(year >= 2020) |>
    count()
  counts <- base_query |>  collect()
  counts_filtered <- base_query |>
    galah_geolocate(wkt, type = "bbox") |>
    collect()
  expect_s3_class(counts_filtered, c("tbl_df", "tbl", "data.frame"))
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  expect_lt(count_1, count_2)
})

test_that("atlas_counts returns species counts", {
  skip_if_offline()
  count_species <- galah_call(type = "species") |>
    count() |>
    collect()
  count_records <- galah_call() |>
    count() |>
    collect()
  expect_s3_class(count_species, c("tbl_df", "tbl", "data.frame"))
  expect_type(count_species$count, "integer")
  expect_gt(count_species$count, 0)
  expect_lt(count_species$count, count_records$count)
})

test_that("species counts work with group_by()", {
  skip_if_offline()
  count_species <- galah_call(type = "species") |>
    identify("Crinia") |>
    filter(year >= 2020) |>
    group_by(year) |>
    arrange(year) |>
    count() |>
    collect()
  count_records <- galah_call() |>
    identify("Crinia") |>
    filter(year >= 2020) |>
    group_by(year) |>
    arrange(year) |>
    count() |>
    collect()
  expect_s3_class(count_species, c("tbl_df", "tbl", "data.frame"))
  expect_type(count_species$count, "integer")
  expect_gte(nrow(count_species), 4)
  expect_true(all(count_species$count > 0))
  expect_true(all(count_species$count < 100))
  expect_true(all(count_records$year == count_species$year))
  expect_true(all(count_records$count >= count_species$count))
})

## BELOW HERE TESTS WILL FAIL

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

# FIXME: check non-piped args work
# FIXME: check `galah_` functions work
# FIXME: check `atlas_counts`