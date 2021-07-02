context("Get occurrence data")

ala_config(email = "ala4r@ala.org.au")

test_that("ala_occurrences check inputs", {
  skip_on_cran()
  expect_error(ala_occurrences(filters =
                                 c(stateProvince = "Australian Capital Territory")))
  expect_error(ala_occurrences())
})

test_that("ala_occurrences gives a nice error for invalid emails", {
  skip_on_cran()
  ala_config(email = "test@test.org.au")
  expect_error(ala_occurrences(taxa = select_taxa("Wurmbea dioica")),
  regexp = "Status code 403 was returned for this occurrence download request. This may be because
  the email you provided is not registered with the ALA. Please check and try again.")
  
  ala_config(email = "")
  expect_error(ala_occurrences(taxa = select_taxa("Wurmbea dioica")))
  ala_config(email = "ala4r@ala.org.au")
})

test_that("ala_occurrences handles filters correctly", {
  # can handle multiword filters
  skip_on_cran()
  expect_equal(
    unique(ala_occurrences(filters = select_filters(
      stateProvince = "Australian Capital Territory",
      basisOfRecord = "FossilSpecimen"),
      columns = select_columns("stateProvince", group = "basic"))$stateProvince),
    "Australian Capital Territory")

  # handles year filters
  expect_true(unique(ala_occurrences(filters = select_filters(
    year = seq(1971, 1981), basisOfRecord = "FossilSpecimen"),
    columns = select_columns("year", group = "basic"))$year %in%
      seq(1971, 1981)))
})

#test_that("ala occurrences gives an error for too many filters", {
 # skip_on_cran()
  # generate a query longer than 2000 characters
  #assertions <- rep(TRUE, nrow(find_fields("assertion")))
  #names(assertions) <- find_fields("assertion")$name
  #filters <- select_filters(assertions)
  #expect_error(ala_occurrences(filters = filters,
   #                            "Too many filters provided."))

#})
test_that("ala occurrences returns requested columns", {
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName")
  filters <- select_filters(year = 1930)
  cols <- select_columns(group = "basic")
  id <- select_taxa("Polytelis swainsonii")$taxon_concept_id
  expect_equal(sort(names(ala_occurrences(taxa = id,
                                          filters = filters,
                                          columns = cols))),
               sort(expected_cols))

  cols <- select_columns("occurrence_status", "latitude", "longitude")
  expect_equal(names(ala_occurrences(taxa = id,
                                     filters = filters,
                                     columns = cols)), c("occurrenceStatus",
                                                         "decimalLatitude",
                                                         "decimalLongitude"))
})

test_that("ala occurrences handles assertion columns and works with data.frame
          input", {
  skip("qa field ignored by ALA")
  id <- select_taxa("Paraparatrechina minutula")
  cols <- select_columns("ZERO_COORDINATE", "eventDate")
  expect_equal(names(ala_occurrences(taxa = id, columns = cols)),
               c("eventDate", "ZERO_COORDINATE"))
})

test_that("ala_occurrences handles wkt area inputs", {
  skip_on_cran()
  locations <- select_locations(readLines("../testdata/short_act_wkt.txt"))
  cols <- select_columns(group = "basic", "stateProvince")
  filters <- select_filters(basisOfRecord = "MachineObservation")
  expect_equal(unique(ala_occurrences(locations = locations,
                                      filters = filters,
                                      columns = cols)$stateProvince),
               "Australian Capital Territory")
})

test_that("ala_occurrences handles sf polygon inputs", {
  skip_on_cran()
  # convert wkt to sfc
  act_shp <- st_as_sfc(readLines("../testdata/short_act_wkt.txt"))
  locations <- select_locations(act_shp)
  filters <- select_filters(basisOfRecord = "MachineObservation")
  expect_equal(unique(ala_occurrences(locations = locations, filters = filters,
                                      columns = select_columns(group = "basic",
                                                  "stateProvince"))$stateProvince),
               "Australian Capital Territory")
})

test_that("ala_occurrences caches data as expected", {
  skip_on_cran()
  ala_config(caching = TRUE, verbose = TRUE)
  taxa <- select_taxa("Wurmbea dioica")
  filters <- select_filters(year = 2000)
  columns <- select_columns(group = "basic", "basisOfRecord")

  # Download data
  occ <- ala_occurrences(taxa = taxa, filters = filters, columns = columns)
  # Re-download data
  expect_message(
    ala_occurrences(taxa = taxa, filters = filters, columns = columns),
    "Using cached file")
  ala_config(caching = FALSE)
})

test_that("ala_occurrences downloads data from a DOI", {
  # Only works for ALA DOIs
  skip_on_cran()
  expect_error(ala_occurrences(doi = "random_doi"))
  doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
  expect_gt(nrow(ala_occurrences(doi = doi)), 0)
})
