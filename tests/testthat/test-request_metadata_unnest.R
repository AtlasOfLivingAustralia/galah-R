test_that("request_metadata() |> unnest() works for type = 'fields'", {
  # no filter provided causes an error
  expect_error({request_metadata() |> 
      unnest() |>
      collapse()}) 
  # an incorrect filter argument errors at `collapse()` (NOT `compute()`)
  expect_error({request_metadata() |> 
      filter(something == 10) |> 
      unnest() |>
      collapse()})
  # passing a correct `type` but not `value` passes `collapse()`...
  x <- request_metadata() |> 
    unnest() |>
    filter(field == unknown) |> 
    collapse()
  skip_if_offline()
  # ...but fails at `compute()`
  expect_error({compute(x)})
  # whole thing works when...
  x <- request_metadata() |> 
    unnest() |> 
    filter(field == basisOfRecord) |> 
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(x), 4)
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "basisOfRecord")
  expect_true(any(x[[1]] == "Human observation"))
})

test_that("request_metadata() |> unnest() works for type = 'lists'", {
  # offline stage
  x <- request_metadata() |> 
    filter(list == dr947) |> 
    unnest() |>
    collapse()
  expect_s3_class(x, "query_set")
  y <- compute(x)
  expect_equal(names(y), c("type", "url"))
  expect_equal(y$type, "metadata/lists-unnest")
  skip_if_offline()
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(z), 10)
  expect_gte(ncol(z), 3)
})

test_that("request_metadata() |> unnest() works for type = 'profiles'", {
  # passing a correct `type` but not `value` passes `collapse()`...
  x <- request_metadata() |>
    unnest() |>
    filter(profile == "something") |> 
    collapse()
  expect_s3_class(x, "query_set")
  # ...but fails at `compute()`
  skip_if_offline()
  expect_error(compute(x))
  # whole thing works when a valid profile is given
  x <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    compute()
  y <- collect(x)
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(y), 3)
  expect_gte(nrow(y), 10)
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `identify()`", {
  x <- request_metadata() |>
    identify("crinia") |>
    unnest() |>
    collapse()
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 2)
  expect_equal(unlist(lapply(x, function(a){a$type})), 
               c("metadata/taxa-single", "metadata/taxa-unnest"))
  skip_if_offline()
  y <- compute(x)
  expect_s3_class(y, "query")
  expect_equal(names(y),
               c("type", "url", "headers"))
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
  expect_s3_class(x, "query_set")
  expect_equal(length(x), 1)
  expect_equal(x[[1]]$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "query")
  expect_equal(names(y),
               c("type", "url", "headers"))
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(z), 3)
  expect_gte(nrow(z), 10)
})
