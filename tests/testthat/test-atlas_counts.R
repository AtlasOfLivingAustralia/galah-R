galah_config(verbose = FALSE)

test_that("`collapse()` doesn't ping an API for type = `'occurrences-count'`", {
  skip_if_offline(); skip_on_ci()
  result <- request_data() |>
    filter(year == 2010) |>
    count() |>
    collapse()
  expect_true(inherits(result, "query"))
  expect_equal(result$type, "data/occurrences-count")
})

test_that("atlas_counts works with no arguments", {
  skip_if_offline(); skip_on_ci()
  count <- atlas_counts()
  expect_s3_class(count, c("tbl_df", "tbl", "data.frame"))
  expect_gt(count$count, 0)
})

test_that("count() |> collect() works with no arguments", {
  skip_if_offline(); skip_on_ci()
  count <- galah_call() |>
    count() |>
    collect()
  expect_s3_class(count, c("tbl_df", "tbl", "data.frame"))
  expect_gt(count$count, 0)
})

test_that("`identify()` reduces the number of records returned by `count()`", {
  skip_if_offline(); skip_on_ci()
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

test_that("`filter()` works with dates", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    filter(species == "Cacatua galerita",
           eventDate >= "2023-01-07T00:00:00Z", 
           eventDate < "2023-01-08T00:00:00Z") |>
    count() |>
    collect()
  expect_type(counts$count, "integer")
})

test_that("`galah_identify()` works with `atlas_counts()`", {
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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

# test added to address Issue #265
test_that("atlas_counts handles `identify()` in combination with `OR` statements in `filter()`", {
  skip_if_offline(); skip_on_ci()
  regions <- c("Dampierland","Wet Tropics")
  counts <- galah_call() |>
    identify("Squamata") |>
    filter(cl1048 %in% regions) |>
    group_by(cl1048) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(counts), 2)
  expect_true(all(regions %in% counts$cl1048))
  expect_true(all(counts$count > 0))
})

test_that("atlas_counts returns same result with filter using `,` and `&`", {
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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

test_that("atlas_counts filters correctly with galah_geolocate/galah_bbox/galah_radius", {
  skip_if_offline(); skip_on_ci()
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
  counts_filtered_radius <- base_query |>
    galah_geolocate(lon = 147,
                    lat = -42.9,
                    radius = 20, 
                    type = "radius") |>
    collect()
  expect_s3_class(counts_filtered, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(counts_filtered_radius, c("tbl_df", "tbl", "data.frame"))
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  count_3 <- counts_filtered_radius$count[1]
  expect_lt(count_1, count_2, count_3)
})

test_that("atlas_counts returns species counts", {
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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

test_that("order of `group_by()` doesn't affect result in `atlas_counts()", {
  # This is a test for Issue #198 raised by @shandiya
  # https://github.com/AtlasOfLivingAustralia/galah-R/issues/198
  skip_if_offline(); skip_on_ci()
  reg <- c("Gibson Desert", 
           "Little Sandy Desert", 
           "Southern Volcanic Plain",
           "Flinders Lofty Block")
  # IBRA then year (with no limit)
  ibra_year <- galah_call() |> 
    filter(cl1048 == reg, 
           year >= 1971,
           year <= 2020) |> 
    group_by(cl1048, year) |>
    arrange(desc(count)) |>
    count() |>
    collect()
  year_ibra <- galah_call() |> 
    filter(cl1048 == reg, 
           year >= 1971,
           year <= 2020) |> 
    group_by(year, cl1048) |> 
    arrange(desc(count)) |>
    count() |>
    collect()
  # we expect these two tibbles to have the same colnames,
  # but in a different order (respecting user-supplied info)
  expect_true(all(colnames(ibra_year) %in% c("year", "cl1048", "count")))
  expect_true(all(colnames(year_ibra) %in% c("year", "cl1048", "count")))
  expect_false(all(colnames(year_ibra) == colnames(ibra_year)))
  # we also expect them to have the same number of rows, and the same total
  expect_equal(nrow(ibra_year), nrow(year_ibra))
  expect_equal(sum(ibra_year$count), sum(year_ibra$count))
  ## FIXME:
  # expect_equal(ibra_year, year_ibra) # this fails,
  ## because `arrange` is not (re-)applied after download,
  ## so rows are not in the same order
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
