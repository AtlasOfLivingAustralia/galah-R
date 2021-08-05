context("Test search_taxonomy")

vcr::use_cassette("search_taxonomy_down_to", {
  test_that("search_taxonomy recurses down the ranks", {
    taxa <- search_taxonomy("Animalia", down_to = "phylum")
    expect_s3_class(taxa, "data.frame")
    expect_equal(names(taxa), c("kingdom", "phylum", "authority", "author"))
  })
})

vcr::use_cassette("search_taxonomy_single", {
  test_that("search taxonomy returns data for a single query", {
    taxa <- search_taxonomy("Animalia")
    expect_s3_class(taxa, "data.frame")
    expect_equal(names(taxa), c("kingdom", "authority"))
  })
})

test_that("search_taxonomy checks inputs", {
  expect_error(search_taxonomy("Animalia", down_to = "king"))
  expect_error(search_taxonomy(), "`search_taxonomy` requires a query to search for")
})

test_that("search_taxonomy checks atlas", {
  galah_config(atlas = "Austria")
  expect_error(search_taxonomy("Animalia", down_to = "phylum"))
  galah_config(atlas = "Australia")
})


