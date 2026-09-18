# set up quiet functions for testing reasons
purrr_collect <- purrr::quietly(collect.data_request)

test_that("`filter()` gives an error for single equals sign", {
  galah_call() |> 
    filter(year = 2010) |>
    expect_error()
})

test_that("`filter()` works with assertions", {
  skip_if_offline(); skip_on_ci()
  count_all <- atlas_counts() |>
    dplyr::pull(count)
  count_invalid_spp <- galah_call() |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME") |>
    count() |>
    collect() |>
    dplyr::pull(count)
  count_valid_spp <- galah_call() |>
    filter(assertions != "INVALID_SCIENTIFIC_NAME") |>
    count() |>
    collect() |>
    dplyr::pull(count)
  expect_lt(count_invalid_spp, count_all)
  expect_lt(count_valid_spp, count_all)
  expect_lt(count_invalid_spp, count_valid_spp)
  expect_equal(count_invalid_spp + count_valid_spp,
               count_all)
})

test_that("`filter()` handles multiple assertions", {
  skip_if_offline(); skip_on_ci()
  # OR statements
  all_records <- atlas_counts() |>
    dplyr::pull(count)
  either_valid <- galah_call() |>
    filter(assertions != c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")) |>
    count() |>
    collect() |>
    dplyr::pull(count)
  either_invalid <- galah_call() |>
    filter(assertions == c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")) |>
    count() |>
    collect() |>
    dplyr::pull(count)
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
    dplyr::pull(count)
  both_valid <- galah_call() |>
    filter(assertions != "INVALID_SCIENTIFIC_NAME",
           assertions !=  "COORDINATE_INVALID") |>
    count() |>
    collect() |>
    dplyr::pull(count)
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

test_that("`filter()` handles assertions and taxa", {
  skip_if_offline(); skip_on_ci()
  problem_families <- galah_call() |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME") |>
    group_by(family) |>
    slice_head(n = 5) |>
    count() |>
    purrr_collect()
  problem_families |>
    purrr::pluck("messages") |>
    stringr::str_detect("Limiting to first 5 of ") |>
    expect_true()
  top_family <- galah_call() |>
    identify(problem_families$result$family[1]) |>
    filter(assertions == "INVALID_SCIENTIFIC_NAME") |>
    group_by(family) |>
    count() |>
    collect() # quiet by default
  expect_equal(problem_families$result$count[1], top_family$count)
})

test_that("`filter()` works for two 'equals' arguments", {  
  filters <- request_data() |>
    filter(year == 2010, basisOfRecord == "HUMAN_OBSERVATION") |>
    purrr::pluck("filter")
  expect_s3_class(filters, c("data_filter", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 2)
  expect_equal(filters$variable, c("year", "basisOfRecord"))
})

test_that("`filter()` works for two arguments of the same variable", {  
  filters <- request_data() |>
    filter(year >= 2010, year <= 2015) |>
    purrr::pluck("filter")
  expect_s3_class(filters, c("data_filter", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(filters), 2)
  expect_equal(">=", filters$logical[1])
})

test_that("`filter()` works with urls", {
  filters <- request_data() |>
    filter(taxonConceptID == "https://biodiversity.org.au/afd/taxa/065f1da4-53cd-40b8-a396-80fa5c74dedd") |>
    purrr::pluck("filter")
  expect_true(nchar(filters$query) > 70)
})

test_that("`filter()` parses '&' correctly", {
  filters <- request_data() |> 
    filter(year >= 2010 & year < 2020) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$value, "2010&2020")
  expect_equal(filters$query, "year:[2010 TO *] AND year:[* TO 2020] AND -(year:\"2020\")")
})

test_that("`filter()` handles numeric queries for text fields", {             
  filters <- request_data() |> 
    filter(cl22 >= "Tasmania") |>
    purrr::pluck("filter")
  expect_equal(filters$query, "cl22:[Tasmania TO *]")
})

test_that("`filter()` handles OR statements", {    
  filters <-  request_data() |> 
    filter(year == 2010 | year == 2020) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$value, "2010|2020")
  expect_equal(filters$query, "((year:\"2010\") OR (year:\"2020\"))")
})

test_that("`filter()` handles OR statements", {   
  filters <- request_data() |>
    filter(raw_scientificName == "Litoria jervisiensis" | 
           raw_scientificName == "Litoria peronii") |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$query, 
               "((raw_scientificName:\"Litoria jervisiensis\") OR (raw_scientificName:\"Litoria peronii\"))")
})

test_that("`filter()` works with 3 OR statements", {
  filters <- request_data() |>
    filter(basisOfRecord == "HumanObservation" | 
           basisOfRecord == "MachineObservation" | 
           basisOfRecord == "PreservedSpecimen") |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$value, "HumanObservation|MachineObservation|PreservedSpecimen")
  expect_equal(filters$query, 
               "((basisOfRecord:\"HumanObservation\") OR (basisOfRecord:\"MachineObservation\") OR (basisOfRecord:\"PreservedSpecimen\"))")
})

test_that("`filter()` handles exclusion", {   
  filters <- request_data() |>
    filter(year >= 2010, year != 2021) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 2)
  expect_equal(filters$query, c("year:[2010 TO *]", "-(year:\"2021\")"))
})

test_that("`filter()` handles multiple exclusions", {
  filters <- request_data() |>
    filter(!(stateProvince == "Victoria" & year == 2021)) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_equal(filters$query, "-(stateProvince:\"Victoria\") AND -(year:\"2021\")")
})

test_that("`filter()` handles three terms at once", {    
  filters <- request_data() |>
    filter(basisOfRecord == "HumanObservation",
           year >= 2010,
           stateProvince == "New South Wales") |>
    purrr::pluck("filter")
  expect_equal(nrow(filters),3)
  expect_equal(filters$query, c("(basisOfRecord:\"HumanObservation\")","year:[2010 TO *]","(stateProvince:\"New South Wales\")"))
})

test_that("`filter()` treats `c()` as an OR for numerics", {
  filters <- request_data() |>
    filter(year == c(2010, 2021)) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
})

test_that("`filter()` treats `c()` as an OR for strings", {
  filters <- request_data() |>
    filter(multimedia == c("Image", "Sound", "Video")) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_true(grepl("multimedia:\"Image\"", filters$query))
})

test_that("`filter()` can take an object as a value", { 
  value <- "2010"
  filters <- request_data() |>
    filter(year == value) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_match(filters$query, "(year:\"2010\")")
})

test_that("`filter()` returns error when equations are passed as a string", {
  request_data() |>
    filter("year == 2010") |>
    expect_error()
})

test_that("`filter()` handles taxonomic queries", {
  skip_if_offline(); skip_on_ci()
  filters <- request_data() |>
    filter(taxonConceptID == search_taxa("Animalia")$taxon_concept_id) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_false(grepl("search_taxa", filters$query))
})

test_that("`filter()` handles taxonomic queries when passed as a string", {
  filters <- request_data() |> 
    filter(taxonConceptID == "https://biodiversity.org.au/afd/taxa/012a1234") |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_false(grepl("search_taxa", filters$query))
  expect_true(grepl("taxonConceptID", filters$query))
})

test_that("`filter()` handles taxonomic exclusions", {
  skip_if_offline(); skip_on_ci()
  filters <- request_data() |>
    filter(taxonConceptID == search_taxa("Animalia")$taxon_concept_id,
           taxonConceptID != search_taxa("Chordata")$taxon_concept_id) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 2)
  expect_false(any(grepl("search_taxa", filters$query)))
})

test_that("`filter()` handles lsid as an input", {
  skip_if_offline(); skip_on_ci()
  ids <- c("https://biodiversity.org.au/afd/taxa/0df99ece-1982-4605-a2b3-4fcb7660ee2b",
           "https://id.biodiversity.org.au/node/apni/2910467",
           "https://id.biodiversity.org.au/node/apni/291047") # wrong id
  query <- galah_call() |>
    filter(year == 2020,
           lsid == ids) |>
    count() |>
    collapse()
  expect_s3_class(query, "query")
  expect_equal(length(query), 5)
  expect_equal(names(query), c("type",
                               "atlas",
                               "url", 
                               "headers",
                               "request"))
})

test_that("`filter()` handles different fields separated by OR", {
  filters <- request_data() |>
    filter(phylum == "Chordata" | kingdom == "Plantae") |>
    purrr::pluck("filter")
  expect_equal(filters$query, "((phylum:\"Chordata\") OR (kingdom:\"Plantae\"))")
})

test_that("`filter()` fails when given invalid AND syntax", {
  request_data() |>
    filter(year >= 2020 & 2021) |>
    expect_error()
})

test_that("`filter()` fails when given invalid OR syntax", {
  request_data() |>
    filter(year == 2020 | 2021) |>
    expect_error()
})

# TODOL test that `filter()` handles between() even with multiple filters
# NOTE: not implemented yet

test_that("`filter()` accepts an OR statement for different fields", {
  filters <- request_data() |>
    filter(year == 2010 | basisOfRecord == "PRESERVED_SPECIMEN") |>
    purrr::pluck("filter")
  expect_true(grepl("year", filters$query) & 
              grepl("basisOfRecord", filters$query))
})

test_that("`filter()` handles is.na()", {
  filters <- request_data() |>
    filter(is.na(eventDate)) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_true(grepl("(*:* AND -eventDate:*)", filters$query))
})

test_that("`filter()` handles is.na() with multiple filters", {
  filters <- request_data() |>
    filter(is.na(eventDate), year > 2010) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 2)
  expect_true(grepl("(*:* AND -eventDate:*)", filters$query[[1]]))
})

test_that("`filter()` handles %in%", {
  list_of_years <- 2020:2022
  filters <- request_data() |>
    filter(year %in% list_of_years) |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 1)
  expect_equal("((year:\"2020\") OR (year:\"2021\") OR (year:\"2022\"))", filters$query[[1]])
})

test_that("`filter()` handles %in% with multiple filters", {
  list_of_years <- 2020:2022
  filters <- request_data() |>
    filter(year %in% list_of_years, cl22 == "Tasmania") |>
    purrr::pluck("filter")
  expect_equal(nrow(filters), 2)
  expect_equal("((year:\"2020\") OR (year:\"2021\") OR (year:\"2022\"))", filters$query[[1]])
})

test_that("`filter()` parses fields correctly with is.na()", {
  skip_if_offline(); skip_on_ci()
  expect_no_error(galah_call() |> 
                    filter(is.na(decimalLongitude)) |> 
                    count() |>
                    collect()
                  )
  expect_no_error(galah_call() |>
                    filter(year == 2001, is.na(decimalLongitude)) |>
                    count() |>
                    collect()
                  )
  expect_no_error(galah_call() |>
                    galah_filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) |>
                    count() |>
                    collect()
                  )
  # misspelled or wrong fields
  expect_error(galah_call() |>
                 filter(is.na(decimalLongitde)) |>
                 count() |>
                 collect()
               )
  expect_error(galah_call() |>
                 filter(year == 2001, is.na(decimalLongitde)) |>
                 count() |>
                 collect()
               )
  expect_error(galah_call() |>
                 filter(is.na(bork) | coordinatencertaintyInMeters <= 1000) |>
                 count() |>
                 collect()
               )
})

test_that("`filter()` handles apostrophes (') correctly", {
  skip_if_offline(); skip_on_ci()
  names <- c("Australia's Virtual Herbarium", 
             "iNaturalist observations",
             "iNaturalist research-grade observations")
  query <- request_data() |>
    filter(datasetName %in% names) |>
    count()
  filters <- purrr::pluck(query, "filter")
  result <- collect(query)
  expect_equal(nrow(result), 1) # returns result
  expect_gte(result$count[1], 1)
  expect_match(filters$query, "\\(datasetName:\\\"Australia's Virtual Herbarium\\\"")
})

test_that("`filter()` handles multiple values with brackets correctly", {
  skip_if_offline(); skip_on_ci()
  query <- request_data() |>
    filter(scientificName == c("Aviceda (Aviceda) subcristata", 
                               "Todiramphus (Todiramphus) sanctus")) |>
    count()
  filters <- purrr::pluck(query, "filter")
  result <- collect(query)
  expect_equal(nrow(result), 1) # returns result
  expect_gte(result$count[1], 1)
  expect_match(filters$query, "\\(scientificName:\\\"Aviceda \\(Aviceda\\) subcristata\\\"\\)")
})

test_that("`filter()` builds correct query with `!`, `%in%`, `c()` and `identify()`", {    
  ibra_subset <- c("Brigalow Belt North", "Brigalow Belt South", "Central Mackay Coast")
  query <- request_data() |> 
    identify("Crinia signifera") |>
    filter(!cl1048 %in% ibra_subset) |>
    count()
  expect_equal(query$filter$query, c("-(cl1048:\"Brigalow Belt North\") OR -(cl1048:\"Brigalow Belt South\") OR -(cl1048:\"Central Mackay Coast\")"))
})

test_that("`filter()` accepts {{}} on lhs of formula", {
  skip_if_offline(); skip_on_ci()
  field <- "species"
  result <- galah_call() |>
    filter({{field}} == "Eolophus roseicapilla") |>
    count() |>
    collect()
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(result), 1)
  expect_gte(result$count[1], 100)
})


test_that("`filter() works for class `metadata_request`", {
  x <- request_metadata() |>
    filter(field == cl22)
  expect_s3_class(x, "metadata_request")
  expect_equal(length(x), 3)
  expect_equal(names(x), c("type", "atlas", "filter"))
  expect_equal(names(x$filter), c("variable", "value"))
})

test_that("`filter()` works for class `files_request`", {
  x <- tibble::tibble(
    id = c(1, 2),
    images = c("1234", "5678"))
  y <- request_files() |>
    filter(media == x)
  expect_s3_class(y, "files_request")
  expect_equal(length(y), 3)
  expect_equal(names(y), c("type", "atlas", "filter"))
  expect_equal(y$filter$data, x)
})

rm(purrr_collect)