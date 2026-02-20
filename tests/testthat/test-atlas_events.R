test_that("`galah_call(type = 'events')` works", {
   x <- galah_call(type = "events")
  expect_equal(names(x), "type")
  expect_equal(x$type, "events")
  expect_s3_class(x, "data_request")
})

test_that("`count()` works for type = 'events'", {
  x <- galah_call(type = "events") |>
    count() |>
    collect()
  expect_equal(nrow(x), 1)
  expect_equal(ncol(x), 1)
  expect_true(is.integer(x$count))
})