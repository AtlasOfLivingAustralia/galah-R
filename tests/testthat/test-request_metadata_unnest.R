test_that("request_metadata() |> unnest() works for type = 'fields'", {
  skip_if_offline(); skip_on_ci()
  # no filter provided causes an error
  expect_error({request_metadata() |> 
      unnest() |>
      collapse()},
      label = "Requests of type `fields-unnest` must supply `filter()`") 
  # an incorrect filter argument errors at `collapse()`
  expect_error({request_metadata() |> 
      filter(something == 10) |> 
      unnest() |>
      collapse()},
      label = "Invalid `type` supplied to `unnest()`")
  # passing a correct `type` but not `value` errors at `collapse()`...
  expect_error(request_metadata() |> 
    unnest() |>
    filter(field == unknown) |> 
    collapse(),
    label = "Can't use fields that don't exist.")
  # supplying `filter()` without unnest() works
  x <- request_metadata() |>
    filter(field == "basisOfRecord") |> 
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 1)
  expect_equal(x$id, "basisOfRecord")
  # whole thing works when...
  y <- request_metadata() |> 
    unnest() |> 
    filter(field == "basisOfRecord") |> 
    collect()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(y), 4)
  expect_equal(ncol(y), 1)
  expect_equal(colnames(y), "basisOfRecord")
  expect_true(any(y[[1]] == "HUMAN_OBSERVATION"))
})

test_that("request_metadata() |> select() |> unnest() works for type = 'fields'", {
  skip_if_offline(); skip_on_ci()
  base_query <- request_metadata() |> 
    filter(field == "basisOfRecord") |>
    unnest()
  x <-  base_query |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(x), 4)
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "basisOfRecord")
  expect_true(any(x[[1]] == "HUMAN_OBSERVATION"))
  y <-  base_query |>
    select(everything()) |>
    collect()
  expect_gt(ncol(y), ncol(x))
  z <- base_query |>
    select(label, count) |>
    collect()
  expect_gt(ncol(z), ncol(x))
  expect_lt(ncol(z), ncol(y))
})

test_that("request_metadata() |> unnest() works for type = 'lists'", {
  skip_if_offline(); skip_on_ci()
  x1 <- request_metadata() |> 
    filter(list == "dr947") |> 
    collect()
  expect_s3_class(x1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x1), 1)
  expect_equal(x1[[1]], "dr947")
  x <- request_metadata() |> 
    filter(list == "dr947") |> 
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(x$type, "metadata/lists-unnest")
  expect_equal(names(x), 
               c("type", "url", "request"))
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_gte(nrow(z), 10)
  expect_gte(ncol(z), 3)
  # now check `everything()`
  xx <- request_metadata() |> 
    filter(list == "dr947") |> 
    unnest() |>
    select(everything()) |>
    collect()
  expect_gt(ncol(xx), ncol(z))
})

test_that("`request_metadata() |> unnest()` fails for invalid profiles", {
  skip_if_offline(); skip_on_ci()
  x <- request_metadata() |>
    unnest() |>
    filter(profile == "something")
  expect_error(collapse(x),
               label = "Unknown profile detected")
})

test_that("`request_metadata() |> unnest() |> collapse()` works for type = profiles", {
  skip_if_offline(); skip_on_ci()
    x1 <- request_metadata() |> 
    filter(profile == "ALA") |> 
    collect()
  expect_s3_class(x1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x1), 1)
  expect_equal(x1$short_name, "ALA")
  x <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(x$type, "metadata/profiles-unnest")
  expect_equal(names(x), 
               c("type", "url", "request"))
})

test_that("request_metadata() |> unnest() works for type = 'profiles'", {
  skip_if_offline(); skip_on_ci()
  x <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(x), 3)
  expect_gte(nrow(x), 10)
  # check `select()` works
  y <- request_metadata()  |>
    filter(profile == "ALA") |>
    unnest() |>
    select(filter, enabled) |>
    collect()
  expect_equal(nrow(x), nrow(y))
  expect_equal(ncol(y), 2)
  # now check `everything()`
  z <- request_metadata() |> 
    filter(profile == "ALA") |> 
    unnest() |>
    select(everything()) |>
    collect()
  expect_gt(ncol(z), ncol(x))
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `identify()`", {
  skip_if_offline(); skip_on_ci()
  x1 <- request_metadata() |> 
    identify("crinia") |>
    collect()
  expect_s3_class(x1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x1), 1)
  expect_equal(x1[[1]], "crinia")
  x <- request_metadata() |>
    identify("crinia") |>
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(length(x), 4)
  expect_equal(names(x), c("type", "url", "headers", "request"))
  expect_equal(x$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(z), 4)
  expect_gte(nrow(z), 10)
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `filter()`", {
  skip_if_offline(); skip_on_ci()
  lookup <- search_taxa("Crinia")$taxon_concept_id
  x <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(length(x), 4)
  expect_equal(names(x), c("type", "url", "headers", "request"))
  expect_equal(x$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(z), 4)
  expect_gte(nrow(z), 10)
})

test_that("`request_metadata() |> unnest() |> select()` works for type = 'taxa'", {
  skip_if_offline(); skip_on_ci()
  lookup <- search_taxa("Crinia")$taxon_concept_id
  # request one column only
  x <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    select(name) |>
    collect()
  # request defaults (4 columns)
  y <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    collect()
  # request all columns
  z <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    select(everything()) |>
    collect()
  # should all have same number of rows
  expect_equal(nrow(x), nrow(y))
  expect_equal(nrow(x), nrow(z))
  # but increasing numbers of columns
  expect_gt(ncol(y), ncol(x))
  expect_gt(ncol(z), ncol(x))
  # 
})
