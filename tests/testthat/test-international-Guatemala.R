# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Guatemala works", {
  expect_message(galah_config(atlas = "Guatemala"))
})

test_that("`show_all(fields)` works for Guatemala", {
  skip_galah()
  x <- show_all(fields) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  # also that fields match those returned by default_columns()
  y <- default_columns() # internal function called by `galah_select()`
  expect_true(all(y %in% x$id))
})

test_that("`show_all(collections)` fails for Guatemala", {
  skip_galah()
  show_all(collections, limit = 10) |>
    expect_error()
})

test_that("`show_all(datasets)` works for Guatemala", {
  skip_galah()
  show_all(datasets, limit = 10) |>
    expect_error()
})

test_that("`show_all(providers)` works for Guatemala", {
  skip_galah()
  show_all(providers, limit = 10) |>
    expect_error()
})

test_that("`show_all(reasons)` works for Guatemala", {
  skip_galah()
  x <- show_all(reasons) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
})

test_that("`show_all(assertions)` works for Guatemala", {
  skip_galah()
  x <- show_all(assertions) |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
})

test_that("`show_all(profiles)` fails for Guatemala", {
  expect_error(show_all(profiles))
})

test_that("`search_all(fields)` works for Guatemala", {
  skip_galah()
  x <- search_all(fields, "year") |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 1)
})

test_that("`search_all(taxa)` runs without error for Guatemala", {
  # there appears to be an error for this service as it returns no data
  skip_galah()
  x <- search_all(taxa, "Plantae") |>
    expect_no_error()
  expect_tibble(x)
  expect_equal(nrow(x), 0)
})

test_that("`show_values()` works for fields for Guatemala", {
  skip_galah()
  quiet_values <- function(...){
    x <- purrr::quietly(show_values)
    x(...)$result
  }
  x <- search_all(fields, "basisOfRecord") |> 
    quiet_values() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 1)
})

test_that("`atlas_counts()` works for Guatemala", {
  skip_galah()
  x <- atlas_counts() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(x$count, 0)
})

test_that("`atlas_counts()` works with type = 'species' for Guatemala", {
  skip_galah()
  x <- atlas_counts(type = "species") |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(x$count, 0)
})

## FIXME: Both queries work, but results are notably different
## update 2023-11-03: still happens, but unclear if an API problem or a data problem
# test_that("atlas_counts works with galah_identify for Guatemala", {
#   skip_galah()
#   result <- galah_call() |>
#     identify("Mammalia") |>
#     count() |>
#     collect()
#   expect_gt(result$count, 1)
#   result2 <- galah_call() |>
#     filter(class == "Mammalia") |>
#     count() |>
#     collect()
#   pc_difference <- sqrt((result2$count - result$count)^2) / result$count
#   expect_lt(pc_difference, 0.1) # i.e. <1% margin of error
# })

test_that("`atlas_counts()` works with `group_by()` for Guatemala", {
  skip_galah()
  x <- galah_call() |>
    filter(year >= 2000) |>
    group_by(basisOfRecord) |>
    count() |>
    collect() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gt(nrow(x), 1)
  expect_equal(names(x),
               c("basisOfRecord", "basisOfRecord_label", "count"))
})


test_that("`atlas_species()` works for Guatemala", {
  skip_galah()
  galah_config(
    atlas = "Guatemala",
    email = "test@ala.org.au", 
    send_email = FALSE)
  x <- galah_call() |>
    filter(year <= 1850) |>
    atlas_species()
  expect_tibble(x)
  expect_gte(nrow(x), 10)
  expect_gte(ncol(x), 10)
})

# test_that("`atlas_occurrences()` works for Guatemala", {
#   skip_galah()
#   galah_config(
#     atlas = "Guatemala",
#     email = "test@ala.org.au", 
#     send_email = FALSE)
#   x <- galah_call() |>
#     filter(year == 1900) |>
#     atlas_occurrences()
#   expect_tibble(x)
#   expect_gte(nrow(x), 5)
# })

quiet_config <- purrr::quietly(galah_config)
quiet_config(atlas = "Australia")
rm(quiet_config)