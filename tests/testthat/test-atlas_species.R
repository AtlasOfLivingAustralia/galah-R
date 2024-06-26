test_that("atlas_species fails nicely if no email is provided", {
  skip_if_offline()
  galah_config(email = "", run_checks = TRUE) # run_checks = FALSE doesn't provide error message
  expect_error(atlas_species(identify = galah_identify("Osphranter")))
  galah_config(email = "ala4r@ala.org.au")
})

test_that("atlas_species returns a tibble", {
  skip_if_offline()
  species <- atlas_species(identify = galah_identify("Osphranter"))
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(species), 1)
})

test_that("atlas_species returns correct results when piped", {
  skip_if_offline()
  galah_config(run_checks = TRUE)
  species <- galah_call() |>
    galah_identify("perameles") |>
    galah_filter(year > 2000) |>
    atlas_species()
  expected_species <- c("Perameles nasuta", "Perameles gunnii", 
                        "Perameles pallescens", "Perameles bougainville")
  expected_cols <- c("taxon_concept_id", "species_name",
                     "scientific_name_authorship", "taxon_rank",
                     "kingdom", "phylum", "class", "order", "family",
                     "genus", "vernacular_name")
  expect_setequal(names(species), expected_cols)
  expect_equal(species$species_name[1:4], expected_species)
  expect_gt(nrow(species), 1)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

test_that("atlas_species returns correct results filtered by galah_geolocate", {
  skip_if_offline()
  galah_config(run_checks = TRUE)
  wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 
147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))"
  species <- galah_call() |>
    galah_identify("perameles") |>
    galah_filter(year > 2000) |>
    galah_geolocate(wkt) |>
    atlas_species()
  expected_species <- c("Perameles gunnii")
  expected_cols <- c("taxon_concept_id", "species_name",
                     "scientific_name_authorship", "taxon_rank",
                     "kingdom", "phylum", "class", "order", "family",
                     "genus", "vernacular_name")
  expect_setequal(names(species), expected_cols)
  expect_equal(species$species_name[1], expected_species)
  expect_gt(nrow(species), 0)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

test_that("atlas_species works when no species are present", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  galah_config(run_checks = TRUE)
  result <- galah_call() |>
    galah_identify("eolophus") |>
    galah_filter(cl1048 == "Kimberley") |>
    atlas_species()
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
})

test_that("collapse -> compute -> collect workflow is functional", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  query <- galah_call(method = "data",
                      type = "species") |>
    galah_identify("perameles") |>
    galah_filter(year > 2000)
  species_collapse <- query |> collapse()
  species_compute <- species_collapse |> compute()
  species_collect <- species_compute |> collect()
  atlas_species <- query |> atlas_species()
  
  expect_s3_class(query, "data_request")
  expect_s3_class(species_collapse, "query")
  expect_s3_class(species_compute, "computed_query")
  expect_s3_class(species_collect, c("tbl_df", "tbl", "data.frame"))
  expect_equal(species_collect, atlas_species)
})

test_that("collapse works when no `filter()` is supplied", {
  # NOTE: this test was added to check for the error: "`speciesID` is not a valid field"
  # this occurred when calling `atlas_species()` because `speciesID` is a facet,
  # but `group_by` wasn't being called, so checks weren't constructed properly
  skip_if_offline()
  wkt <- "POLYGON((73.0 -53, 95.6 -11.5, 105.6 -10.1, 123 -12.1, 130.7 -9.5, 142.2 -9.8, 168.1 -29.05, 159.1 -54.9, 73.0 -53))"
  expect_no_error({x <- galah_call(type = "species") |>
    st_crop(wkt) |>
    collapse()})
  expect_s3_class(x, "query")
})