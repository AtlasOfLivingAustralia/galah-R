context("Test that functions are deprecated")
galah_config(verbose = FALSE)

test_that("select_taxa is deprecated", {
  expect_warning(select_taxa("Microseris lanceolata"))
})

test_that("select_taxa works", {
  deprecated <- select_taxa("Microseris lanceolata")
  expect_equal(nrow(deprecated), 1)
})

test_that("select_columns is deprecated", {
  expect_warning(select_columns(eventDate))
})

test_that("select_columns works", {
  deprecated <- select_columns(eventDate)
  expect_s3_class(deprecated, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(deprecated), 1)
  expect_equal(colnames(deprecated), c("name", "type"))
})

test_that("select_filters is deprecated", { 
  expect_warning(select_filters(year == 2000))
})

test_that("select_filters works", { 
  deprecated <- select_filters(year == 2000)
  expect_equal(nrow(deprecated), 1)
  expect_equal(deprecated[[1]], "year")
})

test_that("select_locations is deprecated", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  expect_warning(select_locations(wkt))
})

test_that("select_locations works", {
  wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
  deprecated <- select_locations(wkt)
  expect_match(select_locations(wkt), "MULTIPOLYGON")
})

test_that("ala_occurrences is deprecated", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au", verbose = FALSE)
  filters <- select_filters(year == 1900)
  cols <- select_columns(group = "basic", stateProvince)
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  locations <- select_locations(poly)
  expect_warning(ala_occurrences(
    filters = filters,
    columns = cols,
    locations = locations))
})

test_that("ala_occurrences works", {
  skip_on_cran()
  galah_config(email = "ala4r@ala.org.au", verbose = FALSE)
  filters <- select_filters(year == 1900)
  cols <- select_columns(group = "basic", stateProvince)
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  locations <- select_locations(poly)
  occ <- ala_occurrences(
    filters = filters,
    columns = cols,
    locations = locations)
  expect_true(
    all(c("scientificName", "stateProvince", "occurrenceStatus") %in% names(occ))
  )
  expect_equal(unique(occ$stateProvince), "New South Wales")
})

test_that("ala_counts is deprecated", {
  skip_on_cran()
  expect_warning(ala_counts())
})

test_that("ala_counts works", {
  skip_on_cran()
  deprecated <- ala_counts()
  expect_gt(deprecated$count, 0)
})

test_that("ala_species is deprecated", {
  skip_on_cran()
  species <- expect_warning(
    ala_species(taxa = select_taxa("Osphranter")))
})

test_that("ala_species works", {
  skip_on_cran()
  species <- ala_species(taxa = select_taxa("Osphranter"))
  expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(species), 1)
})

test_that("ala_taxonomy is deprecated", {
  skip_on_cran()
  expect_warning(
    ala_taxonomy(taxa = select_taxa("fungi"),
                 down_to = "phylum"))
})

test_that("ala_taxonomy works", {
  skip_on_cran()
  deprecated <- ala_taxonomy(taxa = select_taxa("fungi"),
                 down_to = "phylum")
  expect_equal(class(deprecated), c("Node", "R6"))
})

test_that("ala_citation is deprecated", {
  data <- data.frame()
  attr(data, "doi") <- "test-doi"
  expect_warning(ala_citation(data))
})

test_that("ala_citation works", {
  data <- data.frame()
  attr(data, "doi") <- "test-doi"
  deprecated <- ala_citation(data)
  expect_match(deprecated, "test-doi")
})

test_that("find_reasons is deprecated", {
  skip_on_cran()
  expect_warning(find_reasons())
})

test_that("find_reasons works", {
  skip_on_cran()
  deprecated <- find_reasons()
  expect_equal(nrow(deprecated), 13)
})

test_that("find_ranks is deprecated", {
  skip_on_cran()
  expect_warning(find_ranks())
})

test_that("find_ranks works", {
  skip_on_cran()
  deprecated <- find_ranks()
  expect_equal(nrow(deprecated), 69)
})

test_that("find_profiles is deprecated", {
  skip_on_cran()
  expect_warning(find_profiles())
})

test_that("find_profiles works", {
  skip_on_cran()
  deprecated <- find_profiles()
  expect_equal(nrow(deprecated), 7)
})

test_that("find_atlases is deprecated", {
  skip_on_cran()
  deprecated <- expect_warning(find_atlases())
})

test_that("find_atlases works", {
  skip_on_cran()
  deprecated <- find_atlases()
  expect_gt(nrow(deprecated), 1)
})

test_that("ala_config is deprecated", {
  expect_error(ala_config())
})