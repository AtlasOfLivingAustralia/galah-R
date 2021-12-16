context("Test atlas_taxonomy")

test_that("ala_taxonomy checks atlas", {
  galah_config(atlas = "Austria")
  expect_error(atlas_taxonomy(search_taxa("Animalia"), down_to = "phylum"))
  galah_config(atlas = "Australia")
})

test_that("atlas_taxonomy 'taxa' must be specified", {
  expect_error(atlas_taxonomy(down_to = "kingdom"))
})

test_that("atlas_taxonomy 'down_to' must be specified", {
  expect_error(atlas_taxonomy(taxa = search_taxa("Animalia")))
})

test_that("atlas_taxonomy 'taxa' must be passed via search_taxa", {
  expect_error(atlas_taxonomy(taxa = "Animalia", down_to = "phylum"))
})

test_that("atlas_taxonomy requires a single taxon", {
  expect_error(atlas_taxonomy(
    taxa = search_taxa(c("Animalia", "Plantae")),
    down_to = "phylum"))
})

