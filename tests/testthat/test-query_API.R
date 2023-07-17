test_that("query_API() works with a single URL and no other arguments", {
  x <- url_lookup("logger_reasons") |> query_API()
  expect_true(inherits(x, "list"))
  expect_gt(length(x), 1)
  expect_true(all(lengths(x) > 1))
})