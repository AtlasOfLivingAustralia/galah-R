# set up quiet functions for testing reasons
quiet_collect <- function(x){
  purrr_collect <- purrr::quietly(collect.data_request)
  purrr_collect(x) |> 
    purrr::pluck("result")
}
galah_config(email = "ala4r@ala.org.au")

test_that("`group_by()` without `distinct()` returns occurrences, not species", {
  skip_if_offline(); skip_on_ci()
  query <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia")
  expected_n <- query |> 
    count() |>
    quiet_collect() 
  query_final <- query |>
    group_by(speciesID)
  expect_equal(query_final$type, "occurrences")
  x <- quiet_collect(query_final)
  expect_s3_class(x,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), expected_n$count)
  expect_true(length(unique(x$taxonConceptID)) < nrow(x))
})

test_that("distinct() with no arguments and no `group_by()` returns occurrences (i.e. does nothing)", {
  skip_if_offline(); skip_on_ci()
  query <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia")
  expected_n <- query |> 
    count() |>
    quiet_collect() 
  query_final <- query |>
    distinct()
  expect_equal(query_final$type, "occurrences")
  x <- quiet_collect(query_final)
  expect_s3_class(x,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), expected_n$count)
  expect_true(length(unique(x$taxonConceptID)) < nrow(x))
})

test_that("`group_by() |> distinct(.keep_all = FALSE)` uses occurrences-count, but *doesn't* return counts", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia") |>
    group_by(speciesID) |>
    distinct() |>
    quiet_collect()
  expect_s3_class(x,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "speciesID")
  expect_equal(length(unique(x$speciesID)),
               nrow(x))
})

test_that("`group_by() |> distinct(.keep_all = TRUE)` converts type from occurrences to species", {
  skip_if_offline(); skip_on_ci()
  query <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia") |>
    group_by(speciesID) |>
    distinct(.keep_all = TRUE) |>
    collapse()
  expect_equal(query$type, "data/species")
  x <- quiet_collect(query)
  expect_s3_class(x,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(length(unique(x$species)),
               nrow(x))
})

test_that("distinct(variable, .keep_all = FALSE) returns field values", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    distinct(cl11226) |>
    quiet_collect()
  # should return values for that field
  expect_s3_class(result,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(result), "cl11226")
  expect_gte(nrow(result), 10)
})

test_that("`distinct(.keep_all = TRUE)` sets species queries", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("Osphranter") |>
    distinct(speciesID, .keep_all = TRUE) |>
    quiet_collect()
  expect_gte(nrow(result), 4)
  expect_s3_class(result,
                  c("tbl_df", "tbl", "data.frame"))
  expect_contains(colnames(result),
                  c("species", "species_name", "kingdom"))
})

test_that("`distinct(variable) |> count()` can be used to count the number of levels", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("perameles") |>
    distinct(taxonConceptID) |>
    count() |>
    quiet_collect()
  expect_s3_class(result,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(result), 1)
  expect_true(result$count[1] > 1 & result$count[1] < 10)
})

test_that("`group_by(something) |> distinct(speciesID) |> count()` gives grouped number of categories", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |>
    identify("perameles") |>
    group_by(basisOfRecord) |>
    distinct(speciesID) |>
    count() |>
    collect()
  expect_equal(colnames(result),
               c("basisOfRecord", "count"))
  all(result$count < 10) |> 
    expect_true()
})

test_that("`add_count() |> distinct()` adds record counts to each species", {
  skip_if_offline(); skip_on_ci()
  query <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia") |>
    group_by(speciesID) |>
    add_count() |>
    distinct(.keep_all = TRUE) |>
    collapse()
  expect_equal(query$type, "data/species")
  x <- quiet_collect(query)
  expect_s3_class(x,
                  c("tbl_df", "tbl", "data.frame"))
  expect_equal(length(unique(x$speciesID)),
               nrow(x))
})

test_that("add_count() without `distinct()` just adds a column of 1s", {
  skip("not built")
})

rm(quiet_collect)