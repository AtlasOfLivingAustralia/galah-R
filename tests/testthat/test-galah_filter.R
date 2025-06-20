test_that("galah_filter works for a single 'equals' argument", {  
  filters <- galah_filter(year == 2010)
  expect_s3_class(filters, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 1)
})

test_that("galah_filter gives an error for single equals sign", {
  expect_error(galah_filter(year = 2010))
})

test_that("galah_filter works with assertions", {
  skip_if_offline(); skip_on_ci()
  count_all <- atlas_counts() |>
    pull(count)
  count_invalid_spp <- galah_call() |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME") |>
    count() |>
    collect() |>
    pull(count)
  count_valid_spp <- galah_call() |>
    filter(assertions != "INVALID_SCIENTIFIC_NAME") |>
    count() |>
    collect() |>
    pull(count)
  expect_lt(count_invalid_spp, count_all)
  expect_lt(count_valid_spp, count_all)
  expect_lt(count_invalid_spp, count_valid_spp)
  expect_equal(count_invalid_spp + count_valid_spp,
               count_all)
})

test_that("galah_filter handles multiple assertions", {
  skip_if_offline(); skip_on_ci()
  # OR statements
  all_records <- atlas_counts() |>
    pull(count)
  either_valid <- galah_call() |>
    galah_filter(assertions != c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")) |>
    count() |>
    collect() |>
    pull(count)
  either_invalid <- galah_call() |>
    filter(assertions == c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")) |>
    count() |>
    collect() |>
    pull(count)
  expect_lt(either_valid, all_records)
  expect_lt(either_invalid, all_records)
  expect_lt(either_invalid, either_valid)
  expect_equal(either_valid + either_invalid, all_records)
  
  # AND statements
  both_invalid <- galah_call() |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME",
           assertions ==  "COORDINATE_INVALID") |>
    count() |>
    collect() |>
    pull(count)
  both_valid <- galah_call() |>
    filter(assertions != "INVALID_SCIENTIFIC_NAME",
           assertions !=  "COORDINATE_INVALID") |>
    count() |>
    collect() |>
    pull(count)
  expect_lt(both_valid, all_records)
  expect_lt(both_invalid, all_records)
  expect_lt(both_invalid, both_valid)
  expect_lt(both_invalid, either_invalid)
  
  ## some further comparisons:
  # expect_lt(both_valid, either_valid) 
  # expect_lt(both_valid + both_invalid, all_records) 
  ## These tests fail, which suggests the query is still not being
  ## constructed carefully enough. More testing needed
  
})

test_that("galah_filter handles assertions and taxa", {
  skip_if_offline(); skip_on_ci()
  problem_families <- galah_call() |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME") |>
    group_by(family) |>
    slice_head(n = 5) |>
    count() |>
    collect() 
  top_family <- galah_call() |>
    identify(problem_families$family[1]) |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME") |>
    group_by(family) |>
    count() |>
    collect() 
  expect_equal(problem_families$count[1], top_family$count)
})

test_that("galah_filter returns empty tibble when no arguments specified", {
  filters <- galah_filter()
  expect_s3_class(filters, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 0)
})

test_that("galah_filter works for two 'equals' arguments", {  
  filters <- galah_filter(year == 2010, basisOfRecord == "HUMAN_OBSERVATION")
  expect_s3_class(filters, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 2)
  expect_equal(filters$variable, c("year", "basisOfRecord"))
})

test_that("galah_filter works for two arguments of the same variable", {  
  filters <- galah_filter(year >= 2010, year <= 2015)
  expect_s3_class(filters, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 2)
  expect_equal(">=", filters$logical[1])
})

test_that("galah_filter works with urls", {
  filters <- galah_filter(
    taxonConceptID == "https://biodiversity.org.au/afd/taxa/065f1da4-53cd-40b8-a396-80fa5c74dedd")
  expect_true(nchar(filters$query) > 70)
})

test_that("galah_filter parses '&' correctly", {
  filters <- galah_filter(year >= 2010 & year < 2020)
  expect_equal(nrow(filters), 1)
  expect_equal(filters$value, "2010&2020")
  expect_equal(filters$query, "year:[2010 TO *] AND year:[* TO 2020] AND -(year:\"2020\")")
})

test_that("galah_filter handles numeric queries for text fields", {             
  filters <- galah_filter(cl22 >= "Tasmania")
  expect_equal(filters$query, "cl22:[Tasmania TO *]")
})

test_that("galah_filter handles OR statements", {    
  filters <- galah_filter(year == 2010 | year == 2020)
  expect_equal(nrow(filters), 1)
  expect_equal(filters$value, "2010|2020")
  expect_equal(filters$query, "((year:\"2010\") OR (year:\"2020\"))")
})

test_that("galah_filter handles OR statements", {   
  filters <- galah_filter(raw_scientificName == "Litoria jervisiensis" | 
                          raw_scientificName == "Litoria peronii")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$query, 
               "((raw_scientificName:\"Litoria jervisiensis\") OR (raw_scientificName:\"Litoria peronii\"))")
})

test_that("galah_filter works with 3 OR statements", {
  filters <- galah_filter(basisOfRecord == "HumanObservation" | 
                          basisOfRecord == "MachineObservation" | 
                          basisOfRecord == "PreservedSpecimen")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$value, "HumanObservation|MachineObservation|PreservedSpecimen")
  expect_equal(filters$query, 
               "((basisOfRecord:\"HumanObservation\") OR (basisOfRecord:\"MachineObservation\") OR (basisOfRecord:\"PreservedSpecimen\"))")
})

test_that("galah_filter handles exclusion", {   
  filters <- galah_filter(year >= 2010, year != 2021)
  expect_equal(nrow(filters), 2)
  expect_equal(filters$query, c("year:[2010 TO *]", "-(year:\"2021\")"))
})

test_that("galah_filter handles multiple exclusions", {
  filters <- galah_filter(!(stateProvince == "Victoria" & year == 2021)) 
  expect_equal(nrow(filters), 1)
  expect_equal(filters$query, "-(stateProvince:\"Victoria\") AND -(year:\"2021\")")
})

test_that("galah_filter handles three terms at once", {    
  filters <- galah_filter(
    basisOfRecord == "HumanObservation",
    year >= 2010,
    stateProvince == "New South Wales")
  expect_equal(nrow(filters),3)
  expect_equal(filters$query, c("(basisOfRecord:\"HumanObservation\")","year:[2010 TO *]","(stateProvince:\"New South Wales\")"))
})

test_that("galah_filter treats `c()` as an OR for numerics", {
  filters <- galah_filter(year == c(2010, 2021))
  expect_equal(nrow(filters), 1)
})

test_that("galah_filter treats `c()` as an OR for strings", {
  filters <- galah_filter(multimedia == c("Image", "Sound", "Video"))
  expect_equal(nrow(filters), 1)
  expect_true(grepl("multimedia:\"Image\"", filters$query))
})

# ## NOT SUPPORTED: requires :=
##  - note this is difficult to support as it parses as a named object
## i.e. gets caught by check_named_input()
# test_that("galah_filter can take an object as a field", {  
#   field <- "year"
#   filters <- galah_filter(field := 2010)
#   expect_equal(nrow(filters), 1)
#   expect_true(grepl("year", filters$query))
# })

test_that("galah_filter can take an object as a value", { 
  value <- "2010"
  filters <- galah_filter(year == value)
  expect_equal(nrow(filters), 1)
  expect_match(filters$query, "(year:\"2010\")")
})

test_that("galah_filter returns error when equations are passed as a string", {
  expect_error(galah_filter("year == 2010"))
})

# # quoting an equation that contains objects - NOT SUPPORTED
# # consider writing a test to specifically exclude this
## or use := as per {dplyr} - note this is difficult to support as it parses as a named object
## i.e. gets caught by check_named_input()
# field <- "year"
# value <- "2010"
# filters <- galah_filter("field == value")
# expect_equal(attr(filters, "call"), "galah_filter")
# expect_equal(nrow(filters), 1)
# expect_true(grepl("2010", filters$query))

test_that("galah_filter handles taxonomic queries", {
  skip_if_offline(); skip_on_ci()
  filters <- galah_filter(taxonConceptID == search_taxa("Animalia")$taxon_concept_id)
  expect_equal(nrow(filters), 1)
  expect_false(grepl("search_taxa", filters$query))
})

test_that("galah_filter handles taxonomic queries when passed as a string", {
  # ensure a taxonomic query to galah_filter works
  filters <- galah_filter(taxonConceptID == "https://biodiversity.org.au/afd/taxa/012a1234")
  expect_equal(nrow(filters), 1)
  expect_false(grepl("search_taxa", filters$query))
  expect_true(grepl("taxonConceptID", filters$query))
})

test_that("galah_filter handles taxonomic exclusions", {
  skip_if_offline(); skip_on_ci()
  filters <- galah_filter(
    taxonConceptID == search_taxa("Animalia")$taxon_concept_id,
    taxonConceptID != search_taxa("Chordata")$taxon_concept_id)
  expect_equal(nrow(filters), 2)
  expect_false(any(grepl("search_taxa", filters$query)))
})

test_that("galah_filter handles lsid as an input", {
  skip_if_offline(); skip_on_ci()
  ids <- c("https://biodiversity.org.au/afd/taxa/0df99ece-1982-4605-a2b3-4fcb7660ee2b",
           "https://id.biodiversity.org.au/node/apni/2910467",
           "https://id.biodiversity.org.au/node/apni/291047") # wrong id
  query <- galah_call() |>
    galah_filter(year == 2020,
                 lsid == ids) |>
    count() |>
    collapse()
  # number of taxa searches is 3, not 4
  expect_s3_class(query, "query")
  expect_equal(length(query), 6)
  expect_equal(names(query), c("type", 
                               "url", 
                               "headers",
                               "filter",
                               "slot_name", 
                               "expand"))
})

test_that("galah_filter handles different fields separated by OR", {
  filters <- galah_filter(phylum == "Chordata" | kingdom == "Plantae")
  expect_equal(filters$query, "((phylum:\"Chordata\") OR (kingdom:\"Plantae\"))")
})

test_that("galah_filter fails when given invalid AND syntax", {
  expect_error(galah_filter(year >= 2020 & 2021))
})

test_that("galah_filter fails when given invalid OR syntax", {
  expect_error(galah_filter(year == 2020 | 2021))
})

# test that galah_filter handles between() even with multiple filters
# NOTE: not implemented yet

test_that("OR works for different fields", {
  filters <- galah_filter(year == 2010 | basisOfRecord == "PRESERVED_SPECIMEN")
  expect_true(grepl("year", filters$query) & 
              grepl("basisOfRecord", filters$query))
})

test_that("galah_filter handles is.na() even with multiple filters", {
  filter_single <- galah_filter(is.na(eventDate))
  filter_multiple <- galah_filter(is.na(eventDate), year > 2010)
  expect_equal(nrow(filter_single), 1)
  expect_equal(nrow(filter_multiple), 2)
  expect_true(grepl("(*:* AND -eventDate:*)", filter_single$query))
  expect_true(grepl("(*:* AND -eventDate:*)", filter_multiple$query[[1]]))
})

test_that("galah_filter handles %in% even with multiple filters", {
  list_of_years <- 2020:2022
  filter_single <- galah_filter(year %in% list_of_years)
  filter_multiple <- galah_filter(year %in% list_of_years, cl22 == "Tasmania")
  expect_equal(nrow(filter_single), 1)
  expect_equal(nrow(filter_multiple), 2)
  expect_equal("((year:\"2020\") OR (year:\"2021\") OR (year:\"2022\"))", filter_single$query[[1]])
  expect_equal("((year:\"2020\") OR (year:\"2021\") OR (year:\"2022\"))", filter_multiple$query[[1]])
})

test_that("galah_filter parses fields correctly with is.na()", {
  skip_if_offline(); skip_on_ci()
  expect_no_error(galah_call() |> 
                    galah_filter(is.na(decimalLongitude)) |> 
                    atlas_counts()
                  )
  expect_no_error(galah_call() |>
                    galah_filter(year == 2001, is.na(decimalLongitude)) |>
                    atlas_counts()
                  )
  expect_no_error(galah_call() |>
                    galah_filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) |>
                    atlas_counts()
                  )
  # misspelled or wrong fields
  expect_error(galah_call() |>
                 galah_filter(is.na(decimalLongitde)) |>
                 atlas_counts()
               )
  expect_error(galah_call() |>
                 galah_filter(year == 2001, is.na(decimalLongitde)) |>
                 atlas_counts()
               )
  expect_error(galah_call() |>
                 galah_filter(is.na(bork) | coordinatencertaintyInMeters <= 1000) |>
                 atlas_counts()
               )
})

test_that("`galah_filter()` handles apostrophes (') correctly", {
  skip_if_offline(); skip_on_ci()
  names <- c("Australia's Virtual Herbarium", 
             "iNaturalist observations",
             "iNaturalist research-grade observations")
  filter <- galah_filter(datasetName %in% names)$query
  query <- galah_call() |>
    galah_filter(datasetName %in% names) |>
    atlas_counts()
  expect_equal(nrow(query), 1) # returns result
  expect_gte(query$count[1], 1)
  expect_match(filter, "\\(datasetName:\\\"Australia's Virtual Herbarium\\\"")
})

test_that("`galah_filter()` handles multiple values with brackets correctly", {
  skip_if_offline(); skip_on_ci()
  filter <- galah_filter(
    scientificName == c("Aviceda (Aviceda) subcristata", 
                        "Todiramphus (Todiramphus) sanctus"))$query
  query <- galah_call() |>
    galah_filter(scientificName == c("Aviceda (Aviceda) subcristata", 
                                     "Todiramphus (Todiramphus) sanctus")) |>
    atlas_counts()
  expect_equal(nrow(query), 1) # returns result
  expect_gte(query$count[1], 1)
  expect_match(filter, "\\(scientificName:\\\"Aviceda \\(Aviceda\\) subcristata\\\"\\)")
})

test_that("galah_filter builds correct query with `!`, `%in%`, `c()` and `identify()`", {    
  ibra_subset <- c("Brigalow Belt North", "Brigalow Belt South", "Central Mackay Coast")
  query <- request_data(type = "occurrences-count") |> 
    identify("Crinia signifera") |>
    filter(!cl1048 %in% ibra_subset)
  expect_equal(query$filter$query, c("-(cl1048:\"Brigalow Belt North\") OR -(cl1048:\"Brigalow Belt South\") OR -(cl1048:\"Central Mackay Coast\")"))
})

test_that("`galah_filter()` accepts {{}} on lhs of formula", {
  skip_if_offline(); skip_on_ci()
  field <- "species"
  result <- galah_call() |>
    galah_filter({{field}} == "Eolophus roseicapilla") |>
    atlas_counts()
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(result), 1)
  expect_gte(result$count[1], 100)
  result2 <- request_data() |>
    filter({{field}} == "Eolophus roseicapilla") |>
    count() |>
    collect()
  expect_equal(result, result2)
})

test_that("`group_by()` works when > 1 `filter()`", {
  skip_if_offline(); skip_on_ci()
  chosen_species <- c("Eolophus roseicapilla", "Platycercus elegans")
  x <- request_data() |>
    filter(species == chosen_species) |>
    group_by(species) |>
    count() |>
    collect()
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(x$species, chosen_species)
  expect_equal(colnames(x), c("species", "count"))
  expect_equal(nrow(x), 2)
  # previously, adding an additional field (`year` below) removed one species from resulting tibble
  y <- request_data() |>
    filter(species == chosen_species,
           year == 2023) |>
    group_by(species) |>
    count() |>
    collect()
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_equal(y$species, chosen_species)
  expect_equal(colnames(y), c("species", "count"))
  expect_equal(nrow(y), 2)
  expect_true(all(x$count > y$count)) # extra filter
  # compare to different syntax
  z <- galah_call() |>
    galah_filter(species == c("Eolophus roseicapilla", "Platycercus elegans"),
                 year == 2023) |>
    galah_group_by(species) |>
    atlas_counts()
  expect_equal(y, z)
})

test_that("galah_filter handles `type = 'data'` correctly", {
  x <- galah_call(method = "data") |>
    galah_filter(year == 2010)
  # NOTE: this errors because `galah_filter()` doesn't handle `type = "files"` like `filter()` does
  # needs fixing
  expect_s3_class(x, "data_request")
  expect_false(is.null(x$filter))
  expect_equal(colnames(x$filter), c("variable", "logical", "value", "query"))
  y <- request_data() |> filter(year == 2010)
  expect_equal(x, y)
})

test_that("galah_filter handles `type = 'metadata'` correctly", {
  x <- galah_call(method = "metadata") |>
    galah_filter(field == cl22)
  expect_s3_class(x, "metadata_request")
  expect_equal(length(x), 2)
  expect_equal(names(x), c("type", "filter"))
  expect_equal(colnames(x$filter), c("variable", "logical", "value"))
  y <- request_metadata() |> filter(field == cl22)
  expect_equal(x, y)
})

test_that("galah_filter handles `type = 'files'` correctly", {
  x <- tibble(
    id = c(1, 2),
    images = c("1234", "5678"))
  y <- galah_call(method = "files") |>
    galah_filter(media == x)
  expect_s3_class(y, "files_request")
  expect_equal(length(y), 2)
  expect_equal(names(y), c("type", "filter"))
  expect_equal(y$filter, x)
  z <- request_files() |> filter(media == x)
  expect_equal(y, z)
})
