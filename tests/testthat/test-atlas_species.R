context("Test atlas_species")

test_that("atlas_species returns a dataframe", {
  skip_on_cran()
  # galah_config(caching = TRUE)
  species <- atlas_species(identify = galah_identify("Osphranter"))
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(species), 1)
  
  # # check cached results is the same
  # expect_message(species2 <- atlas_species(identify = galah_identify("Osphranter")))
  # expect_equal(species, species2)
  # galah_config(caching = FALSE)
})


test_that("atlas_species returns correct results when piped", {
  skip_on_cran()
  species <- galah_call() |>
    galah_identify("perameles") |>
    galah_filter(year > 2000) |>
    atlas_species()
  expected_species <- c("Perameles nasuta", "Perameles gunnii", 
                        "Perameles pallescens", "Perameles bougainville")
  expected_cols <- c("kingdom", "phylum", "class", "order", "family",
                          "genus", "species", "author", "species_guid", 
                          "vernacular_name")
  expect_setequal(names(species), expected_cols)
  expect_equal(species$species[1:4], expected_species)
  expect_gt(nrow(species), 1)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})
