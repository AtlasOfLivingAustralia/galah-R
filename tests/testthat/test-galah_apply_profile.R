test_that("`apply_profile()` amends a `query", {
  x <- galah_call() |>
    filter (year == 2025) |>
    apply_profile("ALA")
  expect_equal(x$data_profile, "ALA")
})

test_that("`galah_apply_profile()` matches profile", {
  skip_if_offline(); skip_on_ci()
  profile_1 <- galah_apply_profile("ALA")
  profile_2 <- galah_apply_profile(AVH)
  expect_equal(profile_1[[1]],  "ALA")
  expect_equal(profile_2[[1]],  "AVH")
})

test_that("`apply_profile()` errors at `collapse()`", {
  skip_if_offline(); skip_on_ci()
  galah_config(run_checks = TRUE)
  expect_error(request_data() |>
                 apply_profile(whatever) |>
                 count() |>
                 collapse(),
               "Unrecognised profile requested.")
  galah_config(run_checks = FALSE)
})

test_that("`galah_apply_profile()` allows only one profile at a time", {
  skip_if_offline(); skip_on_ci()
  expect_error(galah_apply_profile(ALA, CSDM), 
               "Too many data profiles supplied.")
})

test_that("`apply_profile()` filters counts", {
  skip_if_offline(); skip_on_ci()
  without_profile <- galah_call() |>
    count() |>
    collect()
  with_profile <- galah_call() |>
    apply_profile(ALA) |>
    count() |>
    collect()
  expect_gt(with_profile$count, 0)
  expect_equal(class(without_profile), class(with_profile))
  expect_lt(with_profile$count, without_profile$count)
  # add dplyr syntax
  with_profile_2 <- request_data() |>
    galah_apply_profile(ALA) |>
    atlas_counts()
  expect_equal(with_profile, with_profile_2)
})

test_that("`apply_profile()` filters species", {
  skip_if_offline(); skip_on_ci()
  galah_config(email = "ala4r@ala.org.au", 
               atlas = "Australia", 
               run_checks = FALSE)
  without_profile <- galah_call(type = "species") |>
    identify("Crinia") |>
    select(counts, group = "taxonomy") |>
    collect()
  with_profile <- galah_call(type = "species") |>
    identify("Crinia") |>
    select(counts, group = "taxonomy") |>
    apply_profile(ALA) |>
    collect()
  expect_equal(ncol(with_profile), ncol(without_profile))
  expect_lte(nrow(with_profile), nrow(without_profile))
  expect_true(all(with_profile$count <= without_profile$count))
})

test_that("`apply_profile()` filters occurrences", {
  skip_if_offline(); skip_on_ci()
  quiet_collect <- function(x){
    purrr_collect <- purrr::quietly(collect.data_request)
    purrr_collect(x) |> 
      purrr::pluck("result")
  }
  galah_config(email = "ala4r@ala.org.au", 
               atlas = "Australia", 
               run_checks = FALSE)
  without_profile <- galah_call() |>
    identify("Crinia signifera") |>
    filter(year <= 1980,
           cl22 == "Australian Capital Territory") |>
    quiet_collect()
  with_profile <- galah_call() |>
    identify("Crinia signifera") |>
    filter(year <= 1980,
           cl22 == "Australian Capital Territory") |>
    apply_profile(ALA) |>
    quiet_collect()
  expect_gt(nrow(with_profile), 0)
  expect_lt(nrow(with_profile), nrow(without_profile))
  rm(quiet_collect)
})