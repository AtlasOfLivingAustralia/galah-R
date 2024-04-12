test_that("request_metadata() |> unnest() works for type = 'fields'", {
  skip_if_offline()
  # no filter provided causes an error
  expect_error({request_metadata() |> 
      unnest() |>
      collapse()}) 
  # an incorrect filter argument errors at `collapse()`
  expect_error({request_metadata() |> 
      filter(something == 10) |> 
      unnest() |>
      collapse()})
  # passing a correct `type` but not `value` errors at `collapse()`...
  expect_error(request_metadata() |> 
    unnest() |>
    filter(field == unknown) |> 
    collapse())
  # whole thing works when...
  x <- request_metadata() |> 
    unnest() |> 
    filter(field == basisOfRecord) |> 
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(x), 4)
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "basisOfRecord")
  expect_true(any(x[[1]] == "HUMAN_OBSERVATION"))
})

test_that("request_metadata() |> unnest() works for type = 'lists'", {
  skip_if_offline()
  x <- request_metadata() |> 
    filter(list == dr947) |> 
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(x$type, "metadata/lists-unnest")
  expect_equal(names(x), c("type", "url"))
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(z), 10)
  expect_gte(ncol(z), 3)
})

test_that("request_metadata() |> unnest() works for type = 'profiles'", {
  skip_if_offline()
  x <- request_metadata() |>
    unnest() |>
    filter(profile == "something")
  expect_error(collapse(x))
  y <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    collect()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(y), 3)
  expect_gte(nrow(y), 10)
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `identify()`", {
  skip_if_offline()
  x <- request_metadata() |>
    identify("crinia") |>
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(length(x), 3)
  expect_equal(names(x), c("type", "url", "headers"))
  expect_equal(x$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(z), 3)
  expect_gte(nrow(z), 10)
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `filter()`", {
  skip_if_offline()
  lookup <- search_taxa("Crinia")$taxon_concept_id
  x <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(length(x), 3)
  expect_equal(names(x), c("type", "url", "headers"))
  expect_equal(x$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(z), 3)
  expect_gte(nrow(z), 10)
})