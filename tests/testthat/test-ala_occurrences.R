context("Get occurrence data")

ala_config(email = "ala4r@ala.org.au")

test_that("ala_occurrences check inputs", {
  skip_on_cran()
  expect_error(ala_occurrences(filters =
                                 c(state = "Australian Capital Territory")))
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
      state = "Australian Capital Territory",
      basis_of_record = "FossilSpecimen"),
      columns = select_columns("state", group = "basic"))$state),
    "Australian Capital Territory")

  # handles year filters
  expect_true(unique(ala_occurrences(filters = select_filters(
    year = seq(1971, 1981), basis_of_record = "FossilSpecimen"),
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
                     "data_resource")
  filters <- select_filters(occurrence_decade_i = 1930)
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
  skip_on_cran()
  id <- select_taxa("Paraparatrechina minutula")
  cols <- select_columns("zeroLatitude", "zeroLongitude", "eventDate")
  expect_equal(names(ala_occurrences(taxa = id, columns = cols)),
               c("eventDate", "zeroLatitude", "zeroLongitude"))
})

test_that("ala_occurrences handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  valid_wkt <- "POINT(147.08005201710293 -34.48290525355578)"

  wkt <- readLines("../testdata/long_act_wkt.txt")

  locations <- select_locations(wkt = readLines("../testdata/short_act_wkt.txt"))
  cols <- select_columns(group = "basic", "state")
  filters <- select_filters(basis_of_record = "MachineObservation")
  expect_equal(unique(ala_occurrences(locations = locations,
                                      filters = filters,
                                      columns = cols)$stateProvince),
               "Australian Capital Territory")
})

test_that("ala_occurrences handles sf polygon inputs", {
  skip_on_cran()
  # convert wkt to sfc
  act_shp <- st_as_sfc(readLines("../testdata/short_act_wkt.txt"))
  locations <- select_locations(sf = act_shp)
  filters <- select_filters(basis_of_record = "MachineObservation")
  expect_equal(unique(ala_occurrences(locations = locations, filters = filters,
                                      columns = select_columns(group = "basic",
                                                  "state"))$stateProvince),
               "Australian Capital Territory")
})
