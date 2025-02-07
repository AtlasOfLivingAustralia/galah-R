test_that("`group_by` fields are checked during `collapse()`", {
  galah_config(run_checks = TRUE)
  # `collapse()`  should ping a check when `run_checks = TRUE`
  expect_error({
    request_data(type = "occurrences-count") |> 
      group_by(random_invalid_name) |> 
      collapse()
  })
  skip_if_offline(); skip_on_ci()
  # using `count() |> collect()` is synonymous with `request_data(type = "occurrences-count") |> collect()`
  expect_error({
    request_data() |> 
      group_by(random_invalid_name) |> 
      count() |>
      collect()
  })
  galah_config(run_checks = FALSE)
})

test_that("grouped atlas_counts returns expected output", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    identify("Mammalia") |>
    group_by(basisOfRecord) |>
    atlas_counts()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("basisOfRecord", "count"))
})

test_that("grouped atlas_counts returns expected output when limit != NULL", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    identify("Mammalia") |>
    group_by(basisOfRecord) |>
    count() |>
    slice_head(n = 3) |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("basisOfRecord", "count"))
  expect_equal(nrow(counts), 3)
})

test_that("atlas_counts returns all counts if no limit is provided", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    group_by(basisOfRecord) |> # NOTE: basisOfRecord chosen as prone to breaking
    atlas_counts()             # this code; please do not change it!
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(counts), 5)
})

test_that("atlas_counts returns an empty tibble if number of records = 0", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    filter(year < 1900 & year > 2000) |>
    count() |>
    group_by(species) |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(counts), 0)
  expect_equal(ncol(counts), 0)
})

test_that("grouped atlas_counts for species returns expected output", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    identify("Mammalia") |>
    filter(year == 2020) |>
    group_by(month) |>
    count(type = "species") |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(counts), c("month", "count"))
})

test_that("group_by works for three groups", {
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |>
    identify("cacatuidae") |>
    filter(year >= 2020) |>
    group_by(year, basisOfRecord, stateProvince) |>
    count() |>
    collect()
  expect_s3_class(counts, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(counts), 1)
  expect_true(all(names(counts) %in% 
                    c("basisOfRecord", "year", "stateProvince", "count")))
})

test_that("group_by returns correct information when ID fields are requested", {
  # NOTE: previously these were parsed as the `label` for that field, not the 
  # value itself, hence this test
  skip_if_offline(); skip_on_ci()
  counts <- galah_call() |> 
    identify("pardalotus quadragintus") |> 
    filter(year == 2023) |>
    group_by(species, dataResourceName, dataResourceUid) |> 
    count() |>
    collect()
  expect_equal(colnames(counts),
               c("species", "dataResourceName", "dataResourceUid", "count"))
  expect_false(any(counts$dataResourceName == counts$dataResourceUid))
})

test_that("group_by fails for four groups", {
  skip_if_offline(); skip_on_ci()
  galah_call() |>
    identify("cacatuidae") |>
    filter(year >= 2020) |>
    group_by(year, month, basisOfRecord, stateProvince) |>
    expect_error()
})
