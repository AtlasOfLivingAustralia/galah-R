test_that("atlas_taxonomy 'identify' must be specified", {
  expect_error({
    galah_call() |>
      filter(rank == "kingdom") |>
      atlas_taxonomy()
  })
})

test_that("atlas_taxonomy 'filter' must be specified", {
  expect_error({
    galah_call() |>
      identify("Animalia") |>
      atlas_taxonomy()
  })
})

test_that("atlas_taxonomy requires a single taxon", {
  expect_error({
    galah_call() |>
      identify("Animalia", "Plantae") |>
      filter(rank == "Phylum") |>
      atlas_taxonomy() 
  })
})

test_that("atlas_taxonomy makes a tree when piped", {
  skip_if_offline()
  tree <- galah_call() |>
    identify("fungi") |>
    filter(rank >= phylum) |>
    atlas_taxonomy() 
  expect_s3_class(tree, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(tree), 4)
  expect_gte(nrow(tree), 1)
})

test_that("atlas_taxonomy example runs", {
  skip_if_offline()
  df <- galah_call() |> 
    galah_identify("chordata") |>
    galah_filter(rank == class) |>
    atlas_taxonomy()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 4)
  expect_gte(nrow(df), 1)
})

# FIXME: add test for non-piped usage of `atlas_taxonomy()`
# FIXME: add test for constrain_ids