context("Test search_taxa")

vcr::use_cassette("search_taxa_downto", {
  test_that("search_taxa recurses down the ranks", {
    taxa <- search_taxa("Animalia", downto = "phylum")
    expect_s3_class(taxa, "data.frame")
    expect_equal(names(taxa), c("kingdom", "phylum", "authority", "author"))
  })
})

vcr::use_cassette("search_taxa_ids", {
  test_that("search_taxa works searches for a taxon", {
    taxa <- search_taxa("Mammalia", include_ids = TRUE)
    expect_s3_class(taxa, "data.frame")
    expect_equal(names(taxa), c("kingdom", "kingdom_guid", "phylum",
                                "phylum_guid", "class", "class_guid",
                                "authority"))
  })
})

test_that("search_taxa checks inputs", {
  expect_error(search_taxa("Animalia", downto = "king"))
  expect_error(search_taxa(), "`search_taxa` requires a query to search for")
})

test_that("search_taxa checks atlas", {
  galah_config(atlas = "Austria")
  expect_error(search_taxa("Animalia", downto = "phylum"))
  galah_config(atlas = "Australia")
})