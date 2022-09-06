# select_taxa is deprecated

    Code
      deprecated <- select_taxa("Microseris lanceolata")
      expect_equal(nrow(deprecated), 1)

# select_filters is deprecated

    Code
      deprecated <- select_filters(year == 2000)
      expect_equal(nrow(deprecated), 1)
      expect_equal(deprecated[[1]], "year")

# select_locations is deprecated

    Code
      wkt <- "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))"
      deprecated <- select_locations(wkt)
      expect_match(select_locations(wkt), "MULTIPOLYGON")

# ala_occurrences is deprecated

    Code
      galah_config(email = "ala4r@ala.org.au")
      filters <- select_filters(year == 1900)
      cols <- select_columns(group = "basic", stateProvince)
      poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
      locations <- select_locations(poly)
      galah_config(verbose = FALSE)
      occ <- ala_occurrences(filters = filters, columns = cols, locations = locations)
      expect_equal(names(occ)[c(4, 8)], c("scientificName", "stateProvince"))
      expect_equal(unique(occ$stateProvince), "New South Wales")

# ala_counts is deprecated

    Code
      deprecated <- ala_counts()
      expect_gt(deprecated$count, 0)

# ala_species is deprecated

    Code
      species <- ala_species(taxa = select_taxa("Osphranter"))
      expect_s3_class(species, c("tbl_df", "tbl", "data.frame"))
      expect_gt(nrow(species), 1)

# ala_taxonomy is deprecated

    Code
      deprecated <- ala_taxonomy(taxa = select_taxa("fungi"), down_to = "phylum")
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
      expect_gt(nrow(deprecated), 1)

# ala_config is deprecated

    Code
      options(galah_config = NULL)
      expect_equal(ala_config()$verbose, TRUE)

