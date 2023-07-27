galah_config(verbose = FALSE)

without_internet({
  test_that("`collapse()` doesn't ping an API for type = `'occurrences-count'`", {
    result <- request_data(type = "occurrences-count") |> 
      filter(year == 2010) |> 
      collapse()
    expect_true(inherits(result, "data_query"))
    expect_equal(names(result), 
                 c("type", "url", "slot_name", "expand", "headers"))
  })
})

capture_requests("count_no_args", {
  test_that("atlas_counts works with no arguments", {
    count <- galah_call() |> count()
    expect_gt(count$count, 0)
  })
})

capture_requests("count_identify", {
  test_that("`identify()` reduces the number of records returned by `count()`", {
    counts_mammals <- galah_call() |>
      identify("Mammalia") |>
      count()
    counts_all <- galah_call() |> count()
    expect_type(counts_mammals$count, "integer")
    expect_true(counts_mammals$count < counts_all$count)
  })
})

capture_requests("count_with_multiple_group_by", {
  test_that("`count()` handles multiple 'group by' variables", {
    counts <- galah_call() |>
      filter(year >= 2021) |>
      group_by(year, basisOfRecord) |>
      count()
    expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
    expect_true(all(names(counts) %in% c("year", "basisOfRecord", "count")))    
  })
})

capture_requests("count_group_by_species", {
  test_that("`count()` handles 'species' as a 'group by' variable", {
    counts <- galah_call() |>
      identify("perameles") |>
      filter(year > 2010) |> 
      group_by(species, year) |>
      count()
    expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
    expect_true(all(names(counts) %in% c("year", "species", "count")))    
  })
})

# This currently fails - investigate
capture_requests("count_group_by_taxonConceptID", {
  test_that("atlas_counts handles 'taxonConceptID' as a 'group by' variable", {
    counts <- galah_call() |>
      identify("perameles") |>
      filter(year >= 2010) |> 
      group_by(taxonConceptID, year) |>
      count()    
    expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
    expect_true(all(names(counts) %in% c("year", "taxonConceptID", "count")))
  })
})

## BELOW HERE TESTS WILL FAIL

## `galah_down_to()` not checked
# capture_requests("count_piped_2", {
#   test_that("atlas_counts ignores superflueous piped arguments", {
#     counts <- galah_call() |>
#       filter(year >= 2018) |>
#       group_by(year) |>
#       galah_down_to(species) |>
#       select(taxonConceptID) |>
#       count()  
#     expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
#     expect_equal(names(counts), c("year", "count"))
#     expect_gt(nrow(counts), 0)
#   })
# })

## counts for `type = "species"` not checked
# test_that("atlas_counts returns species counts", {
#   vcr::use_cassette("count_type_species", {
#     counts <- galah_call(type = "species") |> count()
#   })
#   expect_type(counts$count, "integer")
#   expect_gt(counts, 0)
# })

## slice_head() not checked
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

## Spatial not checked
# test_that("atlas_counts filters correctly with galah_geolocate/galah_polygon", {
#   vcr::use_cassette("count_piped_polygon", {
#     wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
#        sf::st_as_sfc()
#     base_query <- galah_call() |>
#       identify("dasyurus") |>
#       filter(year >= 2020)
#     counts <- base_query |> atlas_counts()
#     counts_filtered <- base_query |>
#       galah_geolocate(wkt) |>
#       count()
#   })
#   count_1 <- counts_filtered$count[1]
#   count_2 <- counts$count[1]
#   expect_lt(count_1, count_2)
# })
# 
# test_that("atlas_counts filters correctly with galah_geolocate/galah_bbox", {
#   vcr::use_cassette("count_piped_bbox", {
#     wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
#       sf::st_as_sfc()
#     base_query <- galah_call() |>
#       identify("dasyurus") |>
#       filter(year >= 2020)
#     counts <- base_query |> count()
#     counts_filtered <- base_query |>
#       galah_geolocate(wkt, type = "bbox") |>
#       count()
#   })
#   count_1 <- counts_filtered$count[1]
#   count_2 <- counts$count[1]
#   expect_lt(count_1, count_2)
# })