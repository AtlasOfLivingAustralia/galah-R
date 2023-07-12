galah_config(verbose = FALSE)

vcr::use_cassette("count_startup", {
  test_that("atlas_counts checks group_by field", {
    galah_config(run_checks = TRUE)
    expect_warning(atlas_counts(group_by = galah_group_by("invalid")))
    galah_config(run_checks = FALSE)
  })
})

test_that("atlas_counts works with no arguments", {
  vcr::use_cassette("count_no_args", {
    count <- atlas_counts()
  })
  # atlas_counts with no arguments gives the total number of records in the ALA
  expect_gt(count, 0)
})

test_that("atlas_counts returns expected output", {
  vcr::use_cassette("count_identify", {
    counts <- galah_call() |>
      identify("Mammalia") |>
      count()
  })
  expect_type(counts$count, "integer")
})

test_that("atlas_counts returns species counts", {
  vcr::use_cassette("count_type_species", {
    counts <- galah_call(type = "species") |> count()
  })
  expect_type(counts$count, "integer")
  expect_gt(counts, 0)
})

test_that("atlas_counts handles pagination", {
  vcr::use_cassette("count_with_pagination", {
    counts <- galah_call() |>
      group_by(year) |>
      slice_head(n = 101) |>
      count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(counts), 101)
  expect_equal(names(counts), c("year", "count"))
})

test_that("atlas_counts handles multiple 'group by' variables", {
  vcr::use_cassette("count_with_multiple_group_by", {
    counts <- galah_call() |>
      filter(year >= 2018) |>
      group_by(year, basisOfRecord) |>
      count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(names(counts) %in% c("year", "basisOfRecord", "count")))
})

test_that("atlas_counts handles 'species' as a 'group by' variable", {
  vcr::use_cassette("count_type_species_group_by_2", {
    counts <- galah_call() |>
       identify("perameles") |>
       filter(year > 2010) |> 
       group_by(species, year) |>
       count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(names(counts) %in% c("year", "species", "count")))
})

test_that("atlas_counts handles 'taxonConceptID' as a 'group by' variable", {
  vcr::use_cassette("count_group_by_taxonConceptID", {
    counts <- galah_call() |>
       identify("perameles") |>
       filter(year > 2010) |> 
       group_by(taxonConceptID, year) |>
       count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_true(all(names(counts) %in% c("year", "taxonConceptID", "count")))
})

test_that("atlas_counts ignores superflueous piped arguments", {
  vcr::use_cassette("count_piped_2", {
    counts <- galah_call() |>
      galah_filter(year >= 2018) |>
      galah_group_by(year) |>
      galah_down_to(species) |>
      galah_select(taxonConceptID) |>
      atlas_counts()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("year", "count"))
  expect_gt(nrow(counts), 0)
})

test_that("atlas_counts filters correctly with galah_geolocate/galah_polygon", {
  vcr::use_cassette("count_piped_polygon", {
    wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
       sf::st_as_sfc()
    base_query <- galah_call() |>
      identify("dasyurus") |>
      filter(year >= 2020)
    counts <- base_query |> atlas_counts()
    counts_filtered <- base_query |>
      galah_geolocate(wkt) |>
      count()
  })
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  expect_lt(count_1, count_2)
})

test_that("atlas_counts filters correctly with galah_geolocate/galah_bbox", {
  vcr::use_cassette("count_piped_bbox", {
    wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))" |>
      sf::st_as_sfc()
    base_query <- galah_call() |>
      identify("dasyurus") |>
      filter(year >= 2020)
    counts <- base_query |> count()
    counts_filtered <- base_query |>
      galah_geolocate(wkt, type = "bbox") |>
      count()
  })
  count_1 <- counts_filtered$count[1]
  count_2 <- counts$count[1]
  expect_lt(count_1, count_2)
})