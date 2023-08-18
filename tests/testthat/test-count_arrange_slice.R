test_that("default is to arrange by decending order of count", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    collect()
  expect_true(all(diff(result$count) < 0))
  expect_true(nrow(result) > 7)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
})

test_that("arrange in increasing order of count", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(count) |>
    collect()
  expect_true(all(diff(result$count) > 0))
  expect_true(nrow(result) > 7)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
})

test_that("arrange in decreasing order of count using `desc()`", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(desc(count)) |>
    collect()
  expect_true(all(diff(result$count) < 0))
  expect_true(nrow(result) > 7)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
})

test_that("arrange in increasing order of year", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(year) |>
    collect()
  expect_true(all(diff(as.integer(result$year)) == 1))
  expect_true(nrow(result) > 7)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
})

test_that("arrange in decreasing order of year using `desc()`", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(desc(year)) |>
    collect()
  expect_true(all(diff(as.integer(result$year)) == -1))
  expect_true(nrow(result) > 7)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
})

test_that("`arrange()` by `count` and `slice_head()` work together", {
  skip_if_offline()
  # get lower values (bottom 5)
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(count) |>
    slice_head(n = 5) |>
    collect()
  expect_true(all(diff(result$count) > 0))
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
  # get higher values (top 5)
  result2 <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(desc(count)) |>
    slice_head(n = 5) |>
    collect()
  expect_true(all(diff(result2$count) < 0))
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
  # compare two sets of results
  expect_true(mean(as.integer(result$year)) < mean(as.integer(result2$year)))
})

test_that("`arrange()` by `year` and `slice_head()` work together", {
  skip_if_offline()
  # get lower values (bottom 5)
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(year) |>
    slice_head(n = 5) |>
    collect()
  expect_true(all(diff(as.integer(result$year)) == 1))
  expect_equal(result$year[[1]], "2015")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
  # get higher values (top 5)
  result2 <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year) |>
    count() |>
    arrange(desc(year)) |>
    slice_head(n = 5) |>
    collect()
  expect_true(all(diff(as.integer(result2$year)) == -1))
  expect_false(result2$year[[1]] == "2015")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("year", "count"))
  # compare two sets of results
  expect_true(mean(as.integer(result$year)) < mean(as.integer(result2$year)))
})

test_that("`group_by()` with multiple fields works with `slice_head()`", {
  skip_if_offline()
  result <- galah_call() |>
    filter(year >= 2015) |>
    group_by(year, basisOfRecord) |>
    count() |>
    arrange(desc(count)) |> # NOTE: desc(count) applied within groups only; is this correct?
    slice_head(n = 5) |> # same issue as above
    collect()
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("year", "basisOfRecord", "count"))
  expect_true(all(xtabs(~result$year) == 5))
})
  