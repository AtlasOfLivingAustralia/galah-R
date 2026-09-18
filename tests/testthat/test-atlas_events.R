test_that("`galah_call(type = 'events') is deactivated", {
  expect_error(galah_call(type = "events"))
})


test_that("`galah_call(type = 'events')` works", {
  skip("Not yet enabled")
   x <- galah_call(type = "events")
  expect_equal(names(x), c("type", "atlas"))
  expect_equal(x$type, "events")
  expect_s3_class(x, "data_request")
})

test_that("`count()` works for type = 'events'", {
  skip("Not yet enabled")
  x <- galah_call(type = "events") |>
    count() |>
    collect()
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)
  expect_true(is.integer(x$count))
})

test_that("`describe()` works for type = 'events'", {
  skip("Not yet enabled")
  x <- galah_call(type = "events") |>
    describe() |>
    collect()
  expect_gt(nrow(x), 50)
  expect_equal(ncol(x), 3)
  expect_equal(colnames(x), 
               c("id", "description", "data_type"))
})