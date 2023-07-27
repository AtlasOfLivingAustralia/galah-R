capture_requests("group_by_checks", {
  test_that("`group_by` fields are checked during `compute()`, and not before", {
    galah_config(run_checks = TRUE)
    # `collapse()` should never ping a check
    expect_silent({
      request_data(type = "occurrences-count") |> 
        group_by(invalid) |> 
        collapse()
    })
    # `compute()` should ping a check when `run_checks = TRUE`
    expect_error({
      request_data(type = "occurrences-count") |> 
        group_by(invalid) |> 
        compute()
    })
    # using `count()` is synonymous with `request_data(type = "occurrences-count") |> collect()`
    expect_error({
      request_data() |> 
        group_by(invalid) |> 
        count()})
    galah_config(run_checks = FALSE)
  })
})

test_that("grouped atlas_counts returns expected output", {
  vcr::use_cassette("group_by_counts", {
    counts <- galah_call() |>
      identify("Mammalia") |>
      group_by(basisOfRecord) |>
      count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("basisOfRecord", "count"))
})

test_that("grouped atlas_counts returns expected output when limit != NULL", {
  vcr::use_cassette("group_by_with_limit", {
    counts <- galah_call() |>
      identify("Mammalia") |>
      group_by(basisOfRecord) |>
      slice_head(n = 3) |>
      count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("basisOfRecord", "count"))
  expect_equal(nrow(counts), 3)
})

test_that("atlas_counts returns all counts if no limit is provided", {
  vcr::use_cassette("group_by_no_limit", {
    counts <- galah_call() |>
      group_by(month) |>
      count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(counts), 12)
})

test_that("grouped atlas_counts for species returns expected output", {
  vcr::use_cassette("group_by_with_type_species", {
    counts <- galah_call() |>
      identify("Mammalia") |>
      filter(year == 2020) |>
      group_by(month) |>
      count(type = "species")
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("month", "count"))
})

test_that("group_by works for three groups", {
  vcr::use_cassette("group_by_3_groups", {
    counts <- galah_call() |>
      identify("cacatuidae") |>
      filter(year >= 2020) |>
      group_by(year, basisOfRecord, stateProvince) |>
      count()
  })
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(counts), 1)
  expect_true(all(names(counts) %in% 
                    c("basisOfRecord", "year", "stateProvince", "count")))
})

test_that("group_by fails for four groups", {
  galah_call() |>
    identify("cacatuidae") |>
    filter(year >= 2020) |>
    group_by(year, month, basisOfRecord, stateProvince) |>
    expect_error()
})