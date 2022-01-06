# select_taxa is deprecated

    Code
      deprecated <- select_taxa("Microseris lanceolata")
    Warning <lifecycle_warning_deprecated>
      `select_taxa()` was deprecated in galah 1.4.0.
      Please use `search_taxa()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(nrow(deprecated), 1)

# select_columns is deprecated

    Code
      deprecated <- select_columns(eventDate)
    Warning <lifecycle_warning_deprecated>
      `select_columns()` was deprecated in galah 1.4.0.
      Please use `galah_select()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      correct <- structure(tibble(name = "eventDate", type = "field"))
      class(correct) <- append(class(correct), "galah_select")
      expect_equal(deprecated, correct)
      expect_s3_class(deprecated, "galah_select")

# select_filters is deprecated

    Code
      deprecated <- select_filters(year == 2000)
    Warning <lifecycle_warning_deprecated>
      `select_filters()` was deprecated in galah 1.4.0.
      Please use `galah_filter()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(nrow(deprecated), 1)
      expect_equal(deprecated[[1]], "year")

# select_locations is deprecated

    Code
      wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
      deprecated <- select_locations(wkt)
    Warning <lifecycle_warning_deprecated>
      `select_locations()` was deprecated in galah 1.4.0.
      Please use `galah_geolocate()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_match(select_locations(wkt), "MULTIPOLYGON")

# ala_occurrences is deprecated

    Code
      galah_config(email = "ala4r@ala.org.au")
      filters <- select_filters(year == seq(2018, 2020))
      cols <- select_columns(group = "basic", stateProvince, ZERO_COORDINATE)
      taxa <- select_taxa("Polytelis swainsonii")
      poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
      locations <- select_locations(poly)
      galah_config(verbose = FALSE)
      occ <- ala_occurrences(taxa = taxa, filters = filters, columns = cols, locations = locations)
    Warning <lifecycle_warning_deprecated>
      `ala_occurrences()` was deprecated in galah 1.4.0.
      Please use `atlas_occurrences()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(names(occ)[c(4, 8, 9)], c("scientificName", "stateProvince", "ZERO_COORDINATE"))
      expect_equal(unique(occ$stateProvince), "New South Wales")

# ala_counts is deprecated

    Code
      deprecated <- ala_counts()
    Warning <lifecycle_warning_deprecated>
      `ala_counts()` was deprecated in galah 1.4.0.
      Please use `atlas_counts()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_gt(deprecated, 0)

# ala_species is deprecated

    Code
      species <- ala_species(taxa = select_taxa("Osphranter"))
    Warning <lifecycle_warning_deprecated>
      `ala_species()` was deprecated in galah 1.4.0.
      Please use `atlas_species()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_s3_class(species, "data.frame")
      expect_gt(nrow(species), 1)

# ala_taxonomy is deprecated

    Code
      deprecated <- ala_taxonomy(taxa = select_taxa("fungi"), down_to = "phylum")
    Warning <lifecycle_warning_deprecated>
      `ala_taxonomy()` was deprecated in galah 1.4.0.
      Please use `atlas_taxonomy()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(class(deprecated), c("Node", "R6"))

# ala_citation is deprecated

    Code
      data <- data.frame()
      attr(data, "doi") <- "test-doi"
      deprecated <- ala_citation(data)
    Warning <lifecycle_warning_deprecated>
      `ala_citation()` was deprecated in galah 1.4.0.
      Please use `atlas_citation()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_match(atlas_citation(data), "test-doi")

# find_reasons is deprecated

    Code
      deprecated <- find_reasons()
    Warning <lifecycle_warning_deprecated>
      `find_reasons()` was deprecated in galah 1.4.0.
      Please use `show_all_reasons()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(nrow(deprecated), 13)

# find_cached_files is deprecated

    Code
      galah_config(caching = TRUE)
      atlas_counts(group_by = galah_group_by(biome))
    Output
      # A tibble: 2 x 2
        biome          count
        <chr>          <int>
      1 TERRESTRIAL 93288002
      2 MARINE       3504573
    Code
      expect_type(find_cached_files(), "list")
    Warning <lifecycle_warning_deprecated>
      `find_cached_files()` was deprecated in galah 1.4.0.
      Please use `show_all_cached_files()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      clear_cached_files()
    Message <message>
      Cache filtes deleted: 
      
      C:\Users\KEL329\AppData\Local\Temp\Rtmpikjuto\f1512f804177b9da33bef0c4b6ea14d5.rds
    Code
      galah_config(caching = FALSE)

# find_ranks is deprecated

    Code
      deprecated <- find_ranks()
    Warning <lifecycle_warning_deprecated>
      `find_ranks()` was deprecated in galah 1.4.0.
      Please use `show_all_ranks()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(nrow(deprecated), 69)

# find_profiles is deprecated

    Code
      deprecated <- find_profiles()
    Warning <lifecycle_warning_deprecated>
      `find_profiles()` was deprecated in galah 1.4.0.
      Please use `show_all_profiles()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(nrow(deprecated), 7)

# find_atlases is deprecated

    Code
      deprecated <- find_atlases()
    Warning <lifecycle_warning_deprecated>
      `find_atlases()` was deprecated in galah 1.4.0.
      Please use `show_all_atlases()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
    Code
      expect_equal(nrow(deprecated), 6)

# ala_config is deprecated

    Code
      options(galah_config = NULL)
      expect_equal(ala_config()$verbose, TRUE)
    Warning <lifecycle_warning_deprecated>
      `ala_config()` was deprecated in galah 1.3.0.
      Please use `galah_config()` instead.
      This warning is displayed once every 8 hours.
      Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

