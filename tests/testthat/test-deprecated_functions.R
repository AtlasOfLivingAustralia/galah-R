context("Test that functions are deprecated")
galah_config(verbose = FALSE)

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
    deprecated <- select_columns(eventDate)
    correct <- structure(tibble(name = "eventDate",
                                type = "field"))
    class(correct) <- append(class(correct), "galah_select")
    expect_equal(deprecated, correct)
    expect_s3_class(deprecated, "galah_select")
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
    galah_config(email = "ala4r@ala.org.au")
    filters <- select_filters(year == 1900)
    cols <- select_columns(group = "basic", stateProvince)
    poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
    locations <- select_locations(poly)
    galah_config(verbose = FALSE)
    occ <- ala_occurrences(
      filters = filters,
      columns = cols,
      locations = locations)
    expect_equal(
      names(occ)[c(4, 8)], 
      c("scientificName", "stateProvince"))
    expect_equal(unique(occ$stateProvince), "New South Wales")
  })
})

test_that("ala_counts is deprecated", {
  local_edition(3)
  expect_snapshot({
    deprecated <- ala_counts()
    # atlas_counts with no arguments gives the n records in the ALA
    expect_gt(deprecated$count, 0)
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

## This isn't working, because temp folders always have unique names
# test_that("find_cached_files is deprecated", {
#   local_edition(3)
#   expect_snapshot({
#     galah_config(caching = TRUE)
#     result <- atlas_counts(group_by = galah_group_by(biome))
#     expect_type(find_cached_files(), "list")
#     clear_cached_files()
#     galah_config(caching = FALSE)
#   })
# })

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