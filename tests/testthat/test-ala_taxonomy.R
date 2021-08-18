context("Test ala taxonomy")

test_that("ala_taxonomy 'taxa' must be specified", {
  expect_error(ala_taxonomy(down_to = "kingdom"))
})

test_that("ala_taxonomy 'down_to' must be specified", {
  expect_error(ala_taxonomy(taxa = select_taxa("Animalia")))
})

test_that("ala_taxonomy 'taxa' must be passed via select_taxa", {
  expect_error(ala_taxonomy(taxa = "Animalia", down_to = "phylum"))
})

test_that("ala_taxonomy requires a single taxon", {
  expect_error(ala_taxonomy(
    taxa = select_taxa(c("Animalia", "Plantae")),
    down_to = "phylum"))
})

vcr::use_cassette("ala_taxonomy", {
  test_that("ala_taxonomy returns a Node", {
    result <- ala_taxonomy(taxa = select_taxa("Animalia"), down_to = "phylum")
    expect_s3_class(result, "Node")
  })
})
