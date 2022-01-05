context("Test that functions are deprecated")

test_that("select_taxa is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- select_taxa("Microseris lanceolata")
    expect_equal(nrow(deprecated), 1)
  })
})

test_that("select_columns is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- select_columns("eventDate")
    correct <- structure(data.frame(name = "eventDate", 
                                    type = "field"), 
                         class = c("data.frame", "galah_select"))
    class(correct)
    expect_equal(deprecated, correct)
  })
})

test_that("select_filters is deprecated", { 
  local_edition(3)
  expect_snapshot({
    deprecated <- select_filters(year == 2000)
    expect_equal(nrow(deprecated), 1)
    expect_equal(deprecated[[1]], "year")
  })
})

test_that("select_locations is deprecated", {
  local_edition(3)
  expect_snapshot({
    wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
    deprecated <- select_locations(wkt)
    expect_match(select_locations(wkt), "MULTIPOLYGON")
  })
})

test_that("ala_occurrences is deprecated", {
  skip_on_cran()
  local_edition(3)
  expect_snapshot({
    expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                       "scientificName", "taxonConceptID", "recordID",
                       "dataResourceName", "stateProvince", "ZERO_COORDINATE")
    filters <- select_filters(year == seq(2018, 2020))
    cols <- select_columns(group = "basic", stateProvince, ZERO_COORDINATE)
    taxa <- select_taxa("Polytelis swainsonii")
    poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
    locations <- select_locations(poly)
    galah_config(verbose = FALSE)
    occ <- ala_occurrences(
      taxa = taxa,
      filters = filters,
      columns = cols,
      locations = locations)
    expect_setequal(names(occ), expected_cols)
    expect_equal(unique(occ$stateProvince), "New South Wales")
  })
})

test_that("ala_counts is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- ala_counts()
    # atlas_counts with no arguments gives the n records in the ALA
    expect_gt(deprecated, 0)
  })
})

test_that("ala_species is deprecated", {
  local_edition(3)
  expect_snapshot({
    species <- ala_species(taxa = select_taxa("Osphranter"))
    expect_s3_class(species, "data.frame")
    expect_gt(nrow(species), 1)
  })
})

test_that("ala_taxonomy is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- ala_taxonomy(taxa = select_taxa("fungi"),
                               down_to = "phylum")
    expect_equal(class(deprecated), c("Node", "R6"))
  })
})

# test_that("ala_media is deprecated", { # FIXME
#   skip_on_cran()
#   local_edition(3)
#   expect_snapshot({
#     media_dir <- "test_media"
#     unlink(media_dir, recursive = TRUE)
#     dir.create(media_dir)
#     deprecated <- ala_media(taxa = search_taxa("Microseris lanceolata"),
#                             filter = galah_filter(year == 2019),
#                             download_dir = media_dir)
#     correct <- atlas_media(taxa = search_taxa("Microseris lanceolata"),
#                            filter = galah_filter(year == 2019),
#                            download_dir = media_dir)
#     expect_equal(deprecated, correct)
#   })
# })

test_that("ala_citation is deprecated", {
  local_edition(3)
  expect_snapshot({
    data <- data.frame()
    attr(data, "doi") <- "test-doi"
    deprecated <- ala_citation(data)
    expect_match(atlas_citation(data), "test-doi")
  })
})

test_that("find_reasons is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- find_reasons()
    expect_equal(nrow(deprecated), 13)
  })
})

test_that("find_cached_files is deprecated", {
  local_edition(3)
  expect_snapshot({
    # create some metadata
    dir.create('tmp')
    galah_config(caching = TRUE, cache_directory = 'tmp/')
    atlas_counts(group_by = galah_group_by(biome))
    expect_type(find_cached_files(), "list")
    unlink('tmp', recursive = TRUE)
    galah_config(caching = FALSE)
  })
})

test_that("find_ranks is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- find_ranks()
    expect_equal(nrow(deprecated), 69)
  })
})

test_that("find_profiles is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- find_profiles()
    expect_equal(nrow(deprecated), 7)
  })
})

test_that("find_atlases is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- find_atlases()
    expect_equal(nrow(deprecated), 6)
  })
})

test_that("ala_config is deprecated", {
  local_edition(3)
  expect_snapshot({
    # set to null
    options(galah_config = NULL)
    # check that defaults are used
    expect_equal(ala_config()$verbose, TRUE)
  })
})