test_that("compute validates fields", {
  galah_config(run_checks = TRUE)
  expect_error(
    galah_call() |>
      galah_filter(invalid_filter == 'value') |>
      count() |>
      compute(),
    "Can't use fields that don't exist"
    )
})

test_that("compute validates fields when OR statements are used", {
  galah_config(run_checks = TRUE)
  expect_error(
    galah_call() |>
      galah_filter(invalid_filter == 'value' | year == 2010) |>
      count() |>
      compute(),
    "Can't use fields that don't exist"
    )
})

test_that("compute skips field checks if requested", {
  galah_config(run_checks = FALSE)
  expect_silent(
    galah_call() |>
      galah_filter(random == "filter") |>
      count() |>
      compute()
    )
  galah_config(run_checks = TRUE)
})
