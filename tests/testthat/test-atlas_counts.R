galah_config(verbose = FALSE)

test_that("`atlas_counts()` works with no arguments", {
  skip_galah()
  x <- atlas_counts()
  expect_tibble(x)
  expect_gt(x$count, 0)
})

# FIXME: check non-piped args work
# FIXME: check `galah_` functions work
# FIXME: check `atlas_counts`