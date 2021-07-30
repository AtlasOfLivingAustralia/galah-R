context("Test search_taxa")

vcr::use_cassette("search_taxa", {
  test_that("search taxa recurses down the ranks", {
    taxa <- search_taxa("Animalia", downto = "phylum")
    expect_s3_class(taxa, "data.frame")
    expect_equal(names(taxa), c("kingdom", "phylum"))
  })
})

test_that("search_taxa checks inputs", {
  expect_error(search_taxa("Animalia", downto = "king"))
})