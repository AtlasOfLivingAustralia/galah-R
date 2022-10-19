context("Test ALA data profiles")
# NOTE: vcr not used here as it can't handle dq service

test_that("show_all returns correct information by type", {
  skip_on_cran()
  fields <- show_all(fields)
  reasons <- show_all(reasons)
  
  expect_s3_class(fields, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(reasons, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(fields, show_all_fields())
  expect_equivalent(reasons, show_all_reasons())
  expect_equal(attributes(fields)$call, "show_all_fields")
  expect_equal(attributes(reasons)$call, "show_all_reasons")
  expect_equal(ncol(fields), 4)
  expect_equal(ncol(reasons), 2)
})

test_that("show_all checks for valid type input", {
  expect_error(show_all("nothing"))
  expect_error(show_all(FiElDs))
})

test_that("show_all parses ... correctly", {
  fields1 <- show_all(fields)
  fields2 <- show_all("fields")
  expect_equivalent(fields1, fields2)
})

test_that("show_all_assertions returns assertions", {
  skip_on_cran()
  assertions <- show_all_assertions()
  expect_s3_class(assertions, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(assertions)$call, "show_all_assertions")
  expect_equal(ncol(assertions), 4)
})

test_that("show_all_atlases returns atlases", {
  skip_on_cran()
  atlases <- show_all_atlases()
  expect_s3_class(atlases, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(atlases)$call, "show_all_atlases")
  expect_equal(ncol(atlases), 4)
})

test_that("show_all_apis returns apis", {
  skip_on_cran()
  apis <- show_all_apis()
  expect_s3_class(apis, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(apis)$call, "show_all_apis")
  expect_equal(ncol(apis), 5)
})

test_that("show_all_collections returns collections", {
  skip_on_cran()
  collections <- show_all_collections()
  expect_s3_class(collections, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(collections)$call, "show_all_collections")
  expect_equal(ncol(collections), 3)
})

test_that("show_all_datasets returns datasets", {
  skip_on_cran()
  datasets <- show_all_datasets()
  expect_s3_class(datasets, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(datasets)$call, "show_all_datasets")
  expect_equal(ncol(datasets), 3)
})

test_that("show_all_providers returns providers", {
  skip_on_cran()
  providers <- show_all_providers()
  expect_s3_class(providers, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(providers)$call, "show_all_providers")
  expect_equal(ncol(providers), 3)
})

test_that("show_all_fields returns fields", {
  skip_on_cran()
  fields <- show_all_fields()
  expect_s3_class(fields, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(fields)$call, "show_all_fields")
  expect_equal(ncol(fields), 4)
})

test_that("show_all_licences returns licences", {
  skip_on_cran()
  licences <- show_all_licences()
  expect_s3_class(licences, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(licences)$call, "show_all_licences")
  expect_equal(ncol(licences), 4)
})

test_that("show_all_reasons returns reasons", {
  skip_on_cran()
  reasons <- show_all_reasons()
  expect_s3_class(reasons, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(reasons)$call, "show_all_reasons")
  expect_equal(ncol(reasons), 2)
})

test_that("show_all_ranks returns ranks", {
  skip_on_cran()
  ranks <- show_all_ranks()
  expect_s3_class(ranks, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(ranks)$call, "show_all_ranks")
  expect_equal(ncol(ranks), 2)
})

test_that("show_all_profiles returns profiles", {
  skip_on_cran()
  profiles <- show_all_profiles()
  expect_s3_class(profiles, c("tbl_df", "tbl", "data.frame"))
  expect_equal(attributes(profiles)$call, "show_all_profiles")
  expect_equal(ncol(profiles), 4)
})

# Do cached files belong in this test file?

## Future tests
# show_all_fields returns fields, layers, media, other