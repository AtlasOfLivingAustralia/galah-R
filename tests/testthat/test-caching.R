reset_cache()

test_that("`retrieve_cache()` works without any arugments", {
  x <- retrieve_cache()
  expect_equal(
    names(x),
    c("assertions", "fields", "profiles", "reasons"))
})

test_that("`retrieve_cache()` works with a valid arugment", {
  x <- retrieve_cache("fields")
  expect_true(inherits(x, c("tbl_df", "tbl", "data.frame")))
  expect_equal(nrow(x), 0)
})

test_that("`retrieve_cache()` returns `NULL` with an invalid arugment", {
  x <- retrieve_cache("something")
  expect_true(is.null(x))
})

test_that("`update_cache()` adds arbitrary slots to the cache", {
  update_cache(something = list(1, 2, 3))
  # check 'full' cache contains the requisite information
  expect_equal(
    names(retrieve_cache()),
    c("assertions", "fields", "profiles", "reasons", "something"))
  # check we can return a single slot
  expect_equal(
    retrieve_cache("something"),
    list(1, 2, 3))
  reset_cache()
})

test_that("`overwrite_cache()` overwrites whole cache", {
  overwrite_cache(list(x = 1, y = 2))
  x <- retrieve_cache()
  expect_equal(names(x), c("x", "y"))
  expect_equal(x, list(x = 1, y = 2))
  reset_cache()
})