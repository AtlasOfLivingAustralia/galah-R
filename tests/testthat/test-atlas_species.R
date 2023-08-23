test_that("atlas_species fails nicely if no email is provided", {
  skip_if_offline()
  galah_config(email = "", run_checks = FALSE)
  expect_error(atlas_species(identify = galah_identify("Osphranter")))
  galah_config(email = "ala4r@ala.org.au")
})

test_that("atlas_species returns a tibble", {
  skip_if_offline()
  species <- atlas_species(identify = galah_identify("Osphranter"))
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(species), 1)
})

## FIXME: atlas_species no longer returns additional taxonomic information? eg kingdom, phylum, order etc
test_that("atlas_species returns correct results when piped", {
  skip_if_offline()
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

## FIXME: atlas_species no longer returns additional taxonomic information? eg kingdom, phylum, order etc
test_that("atlas_species returns correct results filtered by galah_geolocate", {
  skip_if_offline()
  wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 
147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))"
  species <- galah_call() |>
    galah_identify("perameles") |>
    galah_filter(year > 2000) |>
    galah_geolocate(wkt) |>
    atlas_species()
  expected_species <- c("Perameles gunnii")
  expected_cols <- c("kingdom", "phylum", "class", "order", "family",
                     "genus", "species", "author", "species_guid", 
                     "vernacular_name")
  expect_setequal(names(species), expected_cols)
  expect_equal(species$species[1], expected_species)
  expect_gt(nrow(species), 0)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

test_that("atlas_species works when no species are present", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au")
  result <- galah_call() |>
    galah_identify("eolophus") |>
    galah_filter(cl1048 == "Kimberley") |>
    atlas_species()
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

# test_that("new workflow is functional", {
#   skip_if_offline()
#   galah_config(email = "ala4r@ala.org.au")
#   query <- galah_call() |>
#     galah_identify("perameles") |>
#     galah_filter(year > 2000)
#   species_collapse <- query |> collapse("species")
#   expect_error(compute(species_collapse))
#   species_collect <- species_collapse |> collect()
#   query |> atlas_species()
# })