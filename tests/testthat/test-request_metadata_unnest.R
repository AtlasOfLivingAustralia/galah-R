test_that("`request_metadata() |> unnest()` fails when `filter()` is missing", {
  skip_galah()
  request_metadata() |> 
    unnest() |>
    collapse() |>
    expect_error(label = "Requests of type `fields-unnest` must supply `filter()`") 
})

test_that("incorrect `filter()` argument errors at `collapse()`", {
  skip_galah()
  request_metadata() |> 
    filter(something == 10)
    unnest() |>
    collapse() |>
    expect_error(label = "Invalid `type` supplied to `unnest()`")
})

test_that("passing a correct `type` but not `value` errors at `collapse()`", {
  skip_galah()
  request_metadata() |> 
    unnest() |>
    filter(field == unknown) |> 
    collapse() |>
    expect_error(label = "Can't use fields that don't exist.")
})

test_that("supplying `filter()` without `unnest()` works", {
  x <- request_metadata() |>
    filter(field == "basisOfRecord") |> 
    collect()
  expect_tibble(x)
  expect_equal(nrow(x), 1)
  expect_equal(x$id, "basisOfRecord")
})

test_that("correctly supplied `filter()` and `unnest()` returns values", { 
  x <- request_metadata() |> 
    filter(field == "basisOfRecord") |>
    unnest() |> 
    collect() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 4)
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "basisOfRecord")
  expect_true(any(x[[1]] == "HUMAN_OBSERVATION"))
})

test_that("request_metadata() |> select() |> unnest() works for type = 'fields'", {
  skip_galah()
  base_query <- request_metadata() |> 
    filter(field == "basisOfRecord") |>
    unnest()
  x <-  base_query |>
    collect() |>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(nrow(x), 4)
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "basisOfRecord")
  expect_true(any(x[[1]] == "HUMAN_OBSERVATION"))
  y <-  base_query |>
    select(everything()) |>
    collect() |>
    skip_unavailable()
  expect_gt(ncol(y), ncol(x))
  z <- base_query |>
    select(label, count) |>
    collect() |>
    skip_unavailable()
  expect_gt(ncol(z), ncol(x))
  expect_lt(ncol(z), ncol(y))
})

test_that("request_metadata() |> unnest() works for type = 'lists'", {
  skip_galah()
  galah_config(verbose = FALSE)

  x1 <- request_metadata() |> 
    filter(list == "dr947") |> 
    collect() |>
    skip_unavailable()
  expect_tibble(x1)
  expect_equal(nrow(x1), 1)
  expect_equal(x1[[1]], "dr947")
  x <- request_metadata() |> 
    filter(list == "dr947") |> 
    unnest() |>
    select(taxon_concept_id, supplied_name, rank) |>
    collapse()
  expect_s3_class(x, "query")
  expect_equal(x$type, "metadata/lists-unnest")
  expect_equal(names(x), 
               c("type", "atlas", "url", "request"))
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y) |>
    skip_unavailable()
  expect_tibble(z)
  expect_gte(nrow(z), 10)
  expect_equal(ncol(z), 3)
  # now check `everything()`
  xx <- request_metadata() |> 
    filter(list == "dr947") |> 
    unnest() |>
    select(everything()) |>
    collect() |>
    skip_unavailable()
  expect_gt(ncol(xx), ncol(z))
})

test_that("`request_metadata() |> unnest()` fails for invalid profiles", {
  skip_galah()
  request_metadata() |>
    unnest() |>
    filter(profile == "something") |>
    collapse() |>
    expect_error(label = "Unknown profile detected")
})

test_that("`request_metadata() |> unnest() |> collapse()` works for type = profiles", {
  skip_galah()
  x <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    collapse() |>
    skip_unavailable()
  expect_s3_class(x, "query")
  expect_equal(x$type, "metadata/profiles-unnest")
  expect_equal(names(x), 
               c("type", "atlas", "url", "request"))
})

test_that("`request_metadata() |> unnest() |> collect()` works for type = profiles", {
  skip_galah()
  x <- request_metadata() |> 
    filter(profile == "ALA") |> 
    collect() |>
    skip_unavailable()
  expect_tibble(x)
  expect_equal(nrow(x), 1)
  expect_equal(x$short_name, "ALA")
})

test_that("request_metadata() |> unnest() works for type = 'profiles'", {
  skip_galah()
  x <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    collect()|>
    skip_unavailable()
  expect_tibble(x)
  expect_gte(ncol(x), 3)
  expect_gte(nrow(x), 10)
  # check `select()` works
  y <- request_metadata()  |>
    filter(profile == "ALA") |>
    unnest() |>
    select(filter, enabled) |>
    collect() |>
    skip_unavailable()
  expect_equal(nrow(x), nrow(y))
  expect_equal(ncol(y), 2)
  # now check `everything()`
  z <- request_metadata() |> 
    filter(profile == "ALA") |> 
    unnest() |>
    select(everything()) |>
    collect() |>
    skip_unavailable()
  expect_gt(ncol(z), ncol(x))
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `identify()`", {
  skip_galah()
  x <- request_metadata() |>
    identify("crinia") |>
    unnest() |>
    collapse() |>
    skip_unavailable()
  expect_s3_class(x, "query")
  expect_equal(length(x), 6)
  expect_equal(names(x), c("type", "atlas", "url", "headers", "request", "supplied_taxon"))
  expect_equal(x$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y) |>
    skip_unavailable()
  expect_tibble(z)
  expect_equal(ncol(z), 4)
  expect_gte(nrow(z), 10)
})

test_that("request_metadata() |> unnest() works for type = 'taxa' using `filter()`", {
  skip_galah()
  lookup <- search_taxa("Crinia")$taxon_concept_id
  x <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    collapse()  |>
    skip_unavailable()
  expect_s3_class(x, "query")
  expect_equal(length(x), 5)
  expect_equal(names(x), c("type", "atlas", "url", "headers", "request"))
  expect_equal(x$type, "metadata/taxa-unnest")
  y <- compute(x)
  expect_s3_class(y, "computed_query")
  z <- collect(y) |>
    skip_unavailable()
  expect_tibble(z)
  expect_equal(ncol(z), 4)
  expect_gte(nrow(z), 10)
})

test_that("`request_metadata() |> unnest() |> select()` works for type = 'taxa'", {
  skip_galah()
  lookup <- search_taxa("Crinia")$taxon_concept_id
  # request one column only
  x <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    select(name) |>
    collect() |>
    skip_unavailable()
  expect_tibble(x)
  # request defaults (4 columns)
  y <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    collect() |>
    skip_unavailable()
  expect_tibble(y)
  # request all columns
  z <- request_metadata() |>
    filter(taxa == lookup) |>
    unnest() |>
    select(everything()) |>
    collect() |>
    skip_unavailable()
  expect_tibble(z)
  # should all have same number of rows
  expect_equal(nrow(x), nrow(y))
  expect_equal(nrow(x), nrow(z))
  # but increasing numbers of columns
  expect_gt(ncol(y), ncol(x))
  expect_gt(ncol(z), ncol(x))
  # 
})
