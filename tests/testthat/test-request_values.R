test_that("request_values() works for type = 'collections'", {
  x <- request_values() |> filter(collection == "co85") |> collapse()
  expect_equal(names(x), c("type", "url"))
  expect_equal(x$type, "collections")
  expect_s3_class(x, "values_query")
  skip_if_offline()
  y <- collect(x)
  expect_gte(ncol(x), 10)
  expect_equal(nrow(x), 1)
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
})

test_that("request_values() works for type = 'datasets'", {
  # offline stage
  x <- request_values() |> filter(dataset == "dr711") |> collapse()
  expect_equal(names(x), c("type", "url"))
  expect_equal(x$type, "datasets")
  expect_s3_class(x, "values_query")
  # and run
  skip_if_offline()
  y <- collect(x)
  expect_gte(ncol(y), 10)
  expect_equal(nrow(y), 1)
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
})

test_that("request_values() works for type = 'lists'", {
  # offline stage
  x <- request_values() |> filter(list == "dr947") |> collapse()
  expect_equal(names(x), c("type", "url"))
  expect_equal(x$type, "lists")
  expect_s3_class(x, "values_query")
  # and run
  skip_if_offline()
  y <- collect(x)
  expect_gte(ncol(y), 10)
  expect_equal(nrow(y), 1)
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
})

test_that("object-oriented workflow performs as expected for `type = 'fields'`", {
  # no filter provided causes an error
  expect_error({request_values() |> collapse()}) 
  # an incorrect filter argument errors at `collapse()` (NOT `compute()`)
  expect_error({request_values() |> filter(something == 10) |> collapse()})
  # passing a correct `type` but not `value` passes `collapse()`...
  x <- request_values() |> filter(field == unknown) |> collapse()
  skip_if_offline()
  # ...but fails at `compute()`
  expect_error({compute(x)})
  # whole thing works when...
  x <- request_values() |> filter(field == basisOfRecord) |> collect()
  expect_gte(nrow(x), 4)
  expect_equal(ncol(x), 1)
  expect_equal(colnames(x), "basisOfRecord")
  expect_true(any(x[[1]] == "Human observation"))
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
})

test_that("request_values() works for type = 'profiles'", {
  # passing a correct `type` but not `value` passes `collapse()`...
  x <- request_values() |> filter(profile == "something") |> collapse()
  expect_equal(names(x), c("type", "url"))
  expect_equal(x$type, "profiles")
  expect_s3_class(x, "values_query")
  # ...but fails at `compute()`
  skip_if_offline()
  expect_error(compute(x))
  # whole thing works when a valid profile is given
  x <- request_values() |> filter(profile == "ALA") |> compute()
  y <- collect(x)
  expect_gte(ncol(y), 3)
  expect_gtel(nrow(y), 10)
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
})

test_that("request_values() works for type = 'providers'", {
  # offline stage
  x <- request_values() |> filter(provider == "dp2786") |> collapse()
  expect_equal(names(x), c("type", "url"))
  expect_equal(x$type, "providers")
  expect_s3_class(x, "values_query")
  # and run
  skip_if_offline()
  y <- collect(x)
  expect_gte(ncol(y), 10)
  expect_equal(nrow(y), 1)
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
})