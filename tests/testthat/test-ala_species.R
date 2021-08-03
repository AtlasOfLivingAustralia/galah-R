context("Test ala_species")

test_that("ala_species returns a dataframe", {
  skip_on_cran()
  galah_config(caching = TRUE)
  species <- ala_species(taxa = select_taxa("Osphranter"))
  expect_s3_class(species, "data.frame")
  expect_gt(nrow(species), 1)
  
  # check cached results is the same
  expect_message(species2 <- ala_species(taxa = select_taxa("Osphranter")))
  expect_equal(species, species2)
  galah_config(caching = FALSE)
})
