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
