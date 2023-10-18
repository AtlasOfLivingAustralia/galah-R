test_that("`galah_down_to()` works (for now) in a pipe", {
  expect_warning({x <- galah_call() |> galah_down_to(order)})
  expect_s3_class(x, "data_request")
  expect_true(!is.null(x$filter))
  expect_equal(colnames(x$filter), 
               c("variable", "logical", "value", "query"))
})

test_that("`galah_down_to()` works (for now) in a pipe", {
  x <- galah_down_to(order)
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(x), 
               c("variable", "logical", "value", "query"))
  expect_equal(nrow(x), 1)
})

test_that("`filter.data_request()` accepts `rank` as an argument", {
  x <- galah_call() |>
    filter(rank == order)
  expect_s3_class(x, "data_request")
  expect_true(!is.null(x$filter))
  expect_equal(colnames(x$filter), 
               c("variable", "logical", "value", "query"))
})