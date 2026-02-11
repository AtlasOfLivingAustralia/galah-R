galah_config(verbose = FALSE)

test_that("`atlas_counts()` works with no arguments", {
  skip_if_offline(); skip_on_ci()
  count <- atlas_counts()
  expect_s3_class(count, c("tbl_df", "tbl", "data.frame"))
  expect_gt(count$count, 0)
})

# FIXME: check non-piped args work
# FIXME: check `galah_` functions work
# FIXME: check `atlas_counts`