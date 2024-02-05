test_that("galah_apply_profile filters counts", {
  skip_if_offline()
  without_profile <- galah_call() |>
    atlas_counts()
  with_profile <- galah_call() |>
    galah_apply_profile(ALA) |>
    atlas_counts()
  expect_gt(with_profile[[1]], 0)
  expect_equal(class(without_profile), class(with_profile))
  expect_lt(with_profile[[1]], without_profile[[1]])
  # add dplyr syntax
  with_profile_2 <- request_data() |>
    apply_profile(ALA) |>
    count() |>
    collect()
  expect_equal(with_profile, with_profile_2)
})

test_that("galah_apply_profile filters occurrences", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au", atlas = "Australia", run_checks = FALSE)
  without_profile <- galah_call() |>
    galah_identify("Acanthorhynchus tenuirostris") |>
    galah_filter(year == 2012) |>
    atlas_occurrences()
  with_profile <- galah_call() |>
    galah_identify("Acanthorhynchus tenuirostris") |>
    galah_filter(year == 2012) |>
    galah_apply_profile(ALA) |>
    atlas_occurrences()
  expect_gt(nrow(with_profile), 0)
  expect_lt(nrow(with_profile), nrow(without_profile))
})

test_that("galah_apply_profile matches profile", {
  skip_if_offline()
  profile_1 <- galah_apply_profile("ALA")
  profile_2 <- galah_apply_profile(AVH)
  expect_equal(profile_1[[1]],  "ALA")
  expect_equal(profile_2[[1]],  "AVH")
})

test_that("`galah_apply_profile()` errors at `collapse()`", {
  skip_if_offline()
  galah_config(run_checks = TRUE)
  expect_error(request_data() |>
    apply_profile(whatever) |>
    count() |>
    collapse(),
    "Unrecognised profile requested.")
  galah_config(run_checks = FALSE)
})

test_that("galah_apply_profile allows only one profile at a time", {
  skip_if_offline()
  expect_error(galah_apply_profile(ALA, CSDM), 
               "Too many data profiles supplied.")
})
