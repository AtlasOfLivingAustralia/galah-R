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
  expect_error(atlas_taxonomy(identify = galah_identify("Animalia")))
})

test_that("atlas_taxonomy 'identify' must be passed via `galah_identify`", {
  expect_error(atlas_taxonomy(identify = "Animalia", 
  down_to = galah_down_to(phylum)))
})

test_that("atlas_taxonomy requires a single taxon", {
  expect_error(atlas_taxonomy(
    identify = galah_identify("Animalia", "Plantae"),
    down_to = galah_down_to(phylum)))
})

test_that("atlas_taxonomy makes a tree when piped", {
  vcr::use_cassette("taxonomy_tree_with_pipes", {
    tree <- ToDataFrameTypeCol(
      galah_call() |>
        galah_identify("fungi") |>
        galah_down_to(phylum) |>
        atlas_taxonomy()
    )
  })
  expect_equal(tree[c(1:3), 2], 
               c("Dikarya", "Ascomycota", "Basidiomycota"))
})

