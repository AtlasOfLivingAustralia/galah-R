context("Test ala taxonomy")

test_that("ala_taxonomy checks atlas", {
  galah_config(atlas = "Austria")
  expect_error(ala_taxonomy(select_taxa("Animalia"), down_to = "phylum"))
  galah_config(atlas = "Australia")
})

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
