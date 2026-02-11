quiet_collect <- function(x){
  quiet_fun <- purrr::quietly(dplyr::collect)
  quiet_fun(x) |>
    purrr::pluck("result")
}

quiet_species <- function(...){
  quiet_fun <- purrr::quietly(atlas_species)
  quiet_fun(...) |>
    purrr::pluck("result")
}

test_that("atlas_species fails nicely if no email is provided", {
  skip_if_offline(); skip_on_ci()
  galah_config(email = "", run_checks = TRUE) # run_checks = FALSE doesn't provide error message
  galah_call() |>
    identify("Osphranter") |>
    atlas_species() |>
    expect_error()
  galah_config(email = "ala4r@ala.org.au")
})

test_that("`atlas_species()` returns a tibble", {
  skip_if_offline(); skip_on_ci()
  species <- galah_call() |>
    identify("Osphranter") |>
    quiet_species()
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(species), 1)
})

test_that("`select()` works for type = 'species' with `counts`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call(type = "species") |>
    identify("Crinia") |>
    select(counts) |>
    quiet_collect()
  expect_equal(colnames(x), c("species_id", "count"))
  expect_gt(nrow(x), 10)
})

test_that("`select()` works for type = 'species' with group = 'taxonomy'", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call(type = "species") |>
    identify("Crinia") |>
    select(counts, lists, group = "taxonomy") |>
    quiet_collect()
  expect_true(all(c("species", "count", "kingdom", "phylum") %in% colnames(x)))
  expect_gt(nrow(x), 10)
})

test_that("`atlas_species()` returns correct results when piped", {
  skip_if_offline(); skip_on_ci()
  galah_config(run_checks = TRUE)
  species <- galah_call() |>
    identify("perameles") |>
    filter(year > 2000) |>
    quiet_species()
  expected_species <- c("Perameles nasuta",
                        "Perameles gunnii", 
                        "Perameles fasciata",
                        "Perameles pallescens",
                        "Perameles bougainville")
  expected_cols <- c("species", "species_name",
                     "scientific_name_authorship", "taxon_rank",
                     "kingdom", "phylum", "class", "order", "family",
                     "genus", "vernacular_name")
  expect_setequal(names(species), expected_cols)
  (expected_species %in% species$species_name) |>
    all() |>
    expect_true()
  expect_gt(nrow(species), 1)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

test_that("`atlas_species()` returns correct results filtered by galah_geolocate", {
  skip_if_offline(); skip_on_ci()
  galah_config(run_checks = TRUE)
  wkt <- "POLYGON ((146.5425 -42.63203, 146.8312 -43.13203, 147.4085 -43.13203, 
147.6972 -42.63203, 147.4085 -42.13203, 146.8312 -42.13203, 146.5425 -42.63203))"
  species <- galah_call() |>
    identify("perameles") |>
    filter(year > 2000) |>
    geolocate(wkt) |>
    quiet_species()
  expected_species <- c("Perameles gunnii")
  expected_cols <- c("species", "species_name",
                     "scientific_name_authorship", "taxon_rank",
                     "kingdom", "phylum", "class", "order", "family",
                     "genus", "vernacular_name")
  expect_setequal(names(species), expected_cols)
  expect_equal(species$species_name[1], expected_species)
  expect_gt(nrow(species), 0)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

test_that("`atlas_species()` works when no species are present", {
  skip_if_offline(); skip_on_ci()
  galah_config(email = "ala4r@ala.org.au")
  galah_config(run_checks = TRUE)
  result <- galah_call() |>
    identify("eolophus") |>
    filter(cl1048 == "Kimberley") |>
    quiet_species()
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(result), 11)
  expect_equal(nrow(result), 0)
})

test_that("collapse -> compute -> collect workflow is functional", {
  skip_if_offline(); skip_on_ci()
  galah_config(email = "ala4r@ala.org.au")
  query <- galah_call(type = "species") |>
    identify("perameles") |>
    filter(year > 2000)
  species_collapse <- query |> collapse()
  species_compute <- species_collapse |> compute()
  species_collect <- species_compute |> quiet_collect()
  atlas_species <- query |> quiet_species()
  
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
  skip_if_offline(); skip_on_ci()
  wkt <- "POLYGON((73.0 -53, 95.6 -11.5, 105.6 -10.1, 123 -12.1, 130.7 -9.5, 142.2 -9.8, 168.1 -29.05, 159.1 -54.9, 73.0 -53))"
  expect_no_error({x <- galah_call(type = "species") |>
    st_crop(wkt) |>
    collapse()})
  expect_s3_class(x, "query")
})

test_that("atlas_species reformats column names when empty tibble is returned", {
  skip_if_offline(); skip_on_ci()
  galah_config(run_checks = TRUE)
  
  # No matching species expected, an empty tibble should be returned
  species <- galah_call() |> 
    identify("sarcopterygii") |> 
    filter(cl1048 == "Wet Tropics") |> 
    quiet_species()
  expected_cols <- c("species", "species_name",
                     "scientific_name_authorship", "taxon_rank",
                     "kingdom", "phylum", "class", "order", "family",
                     "genus", "vernacular_name")
  
  expect_setequal(names(species), expected_cols)
  expect_equal(nrow(species), 0)
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
})

test_that("`atlas_species()` accepts `distinct()` to set the grouping variable", {
  skip_if_offline(); skip_on_ci()
  
  genera <- galah_call() |>
    identify("Limnodynastidae") |>
    distinct(genusID) |>
    quiet_species()
  expect_s3_class(genera, c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(genera) > 4 & nrow(genera) < 10)
  expect_gte(ncol(genera), 10) # test that `.keep_all = TRUE` is not required 
  # as this is implied (and asserted) by calling `atlas_species()`
  all(genera$taxon_rank == "genus") |>
    expect_true()
})

rm(quiet_collect, quiet_species)