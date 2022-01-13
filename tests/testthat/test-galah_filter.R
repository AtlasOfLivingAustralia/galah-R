context("Test galah_filter")

test_that("galah_filter builds data quality filters", {
  # vcr can't handle this request
  # skip_on_cran()
  galah_config(run_checks = FALSE)
  filters <- galah_filter(profile = "ALA")
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 0)
  expect_equal(names(filters), c("variable", "logical", "value", "query"))
  expect_equal(attr(filters, "dq_profile"), "ALA")
  galah_config(run_checks = TRUE)
})


test_that("galah_filter gives an error for invalid profile", {
  expect_error(galah_filter(profile = "bad"))
})

test_that("galah_filter gives an error for single equals sign", {
  expect_error(galah_filter(year = 2010))
})

test_that("galah_filter handles assertion filters", {
  filters <- galah_filter(ZERO_COORDINATE == FALSE)
  expect_s3_class(filters, "data.frame")
  expect_true(grepl("assertions", filters$query))   # FIXME 
})

# negative assertions:
# galah_filter(BASIS_OF_RECORD_INVALID == FALSE)

test_that("galah_filter validates filters", {
  galah_config(run_checks = TRUE)
  expect_warning(galah_filter(invalid_filter == 'value'))
})

test_that("galah_filter skips checks if requested", {
  galah_config(run_checks = FALSE)
  expect_silent(galah_filter(random == "filter"))
  galah_config(run_checks = TRUE)
})

test_that("galah_filter returns empty data.frame when no arguments specified", {
  filters <- galah_filter()
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 0)
})


test_that("galah_filter works for a single 'equals' argument", {  
  filters <- galah_filter(year == 2010)
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 1)
})

test_that("galah_filter works for two 'equals' arguments", {  
  filters <- galah_filter(year == 2010, basisOfRecord == "HUMAN_OBSERVATION")
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 2)
})

test_that("galah_filter works for two arguments of the same variable", {  
  filters <- galah_filter(year >= 2010, year <= 2015)
  expect_s3_class(filters, "data.frame")
  expect_equal(nrow(filters), 2)
  expect_equal(">=", filters$logical[1])
})

test_that("galah_filter treats commas and '&' the same way", {
  filter1 <- galah_filter(year >= 2010 & year < 2020)
  filter2 <- galah_filter(year >= 2010, year < 2020)
  expect_equal(filter1, filter2)
})

test_that("galah_filter handles numeric queries for text fields", {             
  filters <- galah_filter(cl22 >= "Tasmania")
  expect_s3_class(filters, c("data.frame", "galah_filter"))
})

test_that("galah_filter handles OR statements", {    
  filters <- galah_filter(year == 2010 | year == 2021)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
})

test_that("galah_filter handles exclusion", {   
  filters <- galah_filter(year >= 2010, year != 2021)
  expect_equal(nrow(filters), 2)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
})

test_that("galah_filter handles three terms at once", {    
  filters <- galah_filter(
    basisOfRecord == "HumanObservation",
    year >= 2010,
    stateProvince == "New South Wales")
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters),3)
})

test_that("galah_filter treats `c()` as an OR statement", {
  filters <- galah_filter(year == c(2010, 2021))
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
})

test_that("galah_filter can take an object as a field", {  
  field <- "year"
  filters <- galah_filter(field == 2010)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("year", filters$query))
})

test_that("galah_filter can take an object as a value", { 
  value <- "2010"
  filters <- galah_filter(year == value)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_match(filters$query, "(year:\"2010\")")
})

test_that("galah_filter can take an object with length >1 as a value", { 
  years <- c(2010, 2021)
  filters <- galah_filter(year == years)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
})

test_that("galah_filter can take an object as an equation", { 
  input_text <- "year == 2010"
  filters <- galah_filter(input_text)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
})

test_that("galah_filter can take an object from a list", { 
  input <- list("year == 2010")
  filters <- galah_filter(input[[1]])
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
})

test_that("galah_filter can accept an equation built with `paste`", { 
  filters <- galah_filter(paste("year", "2010", sep = " == "))
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("2010", filters$query))
})

# # quoting an equation that contains objects - NOT SUPPORTED
# # consider writing a test to specifically exclude this
# field <- "year"
# value <- "2010"
# filters <- galah_filter("field == value")
# expect_s3_class(filters, c("data.frame", "galah_filter"))
# expect_equal(nrow(filters), 1)
# expect_true(grepl("2010", filters$query))

test_that("galah_filter handles taxonomic queries", {
  # ensure a taxonomic query to galah_filter works
  filters <- galah_filter(taxonConceptID == search_taxa("Animalia")$taxon_concept_id)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 1)
  expect_false(grepl("search_taxa", filters$query))
})

test_that("galah_filter handles taxonomic exclusions", {
  filters <- galah_filter(
    taxonConceptID == search_taxa("Animalia")$taxon_concept_id,
    taxonConceptID != search_taxa("Chordata")$taxon_concept_id)
  expect_s3_class(filters, c("data.frame", "galah_filter"))
  expect_equal(nrow(filters), 2)
  expect_false(any(grepl("search_taxa", filters$query)))
})

test_that("galah_filter fails when given invalid AND syntax", {
  filters <- galah_filter(year >= 2020 & 2021)
  expect_equal(nrow(filters), 1)
  expect_false(any(filters$value == 2021))
})

test_that("galah_filter fails when given invalid OR syntax", {
  filters <- galah_filter(year == 2020 | 2021)
  expect_equal(nrow(filters), 1)
  expect_false(any(filters$value == 2021))
  expect_false(grepl("OR", filters$query))
})

test_that("galah_filter handles concatenated strings", {
  filters <- galah_filter(multimedia == c("Image", "Sound", "Video"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("multimedia:\"Image\"", filters$query))
})

test_that("galah_filter handles concatenated numerics", {
  filters <- galah_filter(year == c(2010, 2015, 2020))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("year:\"2010\"", filters$query))
})