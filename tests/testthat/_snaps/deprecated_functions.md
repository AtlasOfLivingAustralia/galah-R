# select_taxa is deprecated

    Code
      deprecated <- search_taxa("Microseris lanceolata")
      expect_equal(nrow(deprecated), 1)

# select_columns is deprecated

    Code
      deprecated <- select_columns("eventDate")
      correct <- structure(data.frame(name = "eventDate", type = "field"), class = c("data.frame", "galah_select"))
      class(correct)
    Output
      [1] "data.frame"   "galah_select"
    Code
      expect_equal(deprecated, correct)

# select_filters is deprecated

    Code
      deprecated <- select_filters(year == 2000)
      expect_equal(nrow(deprecated), 1)
      expect_equal(deprecated[[1]], "year")

# select_locations is deprecated

    Code
      wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
      deprecated <- select_locations(wkt)
      expect_match(galah_geolocate(wkt), "MULTIPOLYGON")

# ala_occurrences is deprecated

    Code
      expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate", "scientificName", "taxonConceptID", "recordID",
        "dataResourceName", "stateProvince", "ZERO_COORDINATE")
      filters <- galah_filter(year == seq(2018, 2020))
      cols <- galah_select(group = "basic", stateProvince, ZERO_COORDINATE)
      taxa <- search_taxa("Polytelis swainsonii")
      poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
      locations <- galah_geolocate(poly)
      occ <- atlas_occurrences(taxa = taxa, filter = filters, select = cols, geolocate = locations)
    Output
        |                                                                                                                        |                                                                                                                |   0%  |                                                                                                                        |================================================================================================================| 100%
    Code
      expect_setequal(names(occ), expected_cols)
      expect_equal(unique(occ$stateProvince), "New South Wales")

# ala_counts is deprecated

    Code
      deprecated <- ala_counts()
      expect_gt(deprecated, 0)

# ala_species is deprecated

    Code
      species <- atlas_species(taxa = search_taxa("Osphranter"))
      expect_s3_class(species, "data.frame")
      expect_gt(nrow(species), 1)

# ala_taxonomy is deprecated

    Code
      deprecated <- ala_taxonomy(taxa = search_taxa("fungi"), down_to = "phylum")
      expect_equal(class(deprecated), c("Node", "R6"))

# ala_citation is deprecated

    Code
      data <- data.frame()
      attr(data, "doi") <- "test-doi"
      deprecated <- ala_citation(data)
      expect_match(atlas_citation(data), "test-doi")

# find_reasons is deprecated

    Code
      deprecated <- find_reasons()
      expect_equal(nrow(deprecated), 13)

# find_cached_files is deprecated

    Code
      dir.create("tmp")
      galah_config(caching = TRUE, cache_directory = "tmp/")
      atlas_counts(group_by = galah_group_by(biome))
    Message <simpleMessage>
      Writing to cache file 'tmp//f1512f804177b9da33bef0c4b6ea14d5.rds'
    Output
              biome    count
      1 TERRESTRIAL 93236664
      2      MARINE  3504507
    Code
      expect_type(show_all_cached_files(), "list")
      unlink("tmp", recursive = TRUE)
      galah_config(caching = FALSE)

# find_ranks is deprecated

    Code
      deprecated <- find_ranks()
      expect_equal(nrow(deprecated), 69)

# find_profiles is deprecated

    Code
      deprecated <- find_profiles()
      expect_equal(nrow(deprecated), 7)

# find_atlases is deprecated

    Code
      deprecated <- find_atlases()
      expect_equal(nrow(deprecated), 6)

# ala_config is deprecated

    Code
      options(ala_config = NULL)
      expect_equal(ala_config()$verbose, TRUE)

