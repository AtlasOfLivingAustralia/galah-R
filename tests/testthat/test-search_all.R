test_that("`search_all()` checks inputs, returns helpful error", {
  skip_galah()
  expect_error(search_all(attributes, ""), "Unrecognised metadata requested")
})

test_that("`search_all(fields)` returns correct output", {
  skip_galah()
  x <- search_all(fields, "year")
  expect_equal(x, search_fields("year"))
  expect_tibble(x)
  expect_equal(attributes(x)$call, "fields")
  expect_equal(ncol(x), 3)
})

test_that("`search_all(reasons)` returns correct output", {
  skip_galah()
  x <- search_all(reasons, "genus")
  expect_equal(x, search_fields("genus"))
  expect_tibble(x)
  expect_equal(attributes(x)$call, "reasons")
  expect_equal(ncol(x), 2)
})

test_that("`search_all(reasons)` returns correct output", {
  skip_galah()
  x <- search_all(reasons, "genus")
  expect_equal(x, search_reasons("genus"))
  expect_tibble(x)
  expect_equal(attributes(x)$call, "reasons")
  expect_equal(ncol(x), 2)
})

test_that("`search_all(profiles)` returns correct output", {
  skip_galah()
  x <- search_all(profiles, "ala")
  expect_equal(x, search_profiles("ala"))
  expect_tibble(x)
  expect_equal(attributes(x)$call, "profiles")
  expect_equal(ncol(x), 4)
})

test_that("`search_all()` returns error when missing query", {
  skip_galah()
  expect_error(search_all(profiles), "We didn't detect a search")
  expect_error(search_all(fields, blah))
})

test_that("`search_assertions()` returns a filtered result", {
  skip_galah()
  all <- show_all_assertions()
  search <- search_assertions("INVALID")
  search2 <- search_all(assertions, "INVALID")
  search_result_check <- all(grepl(pattern = "invalid", 
                                   paste(search$description, search$id),
                                   ignore.case = TRUE))
  
  expect_tibble(search)
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "assertions")
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("`search_apis()` returns a filtered result", {
  search_string <- "image"
  result <- search_all(apis, search_string)
  result |>
    dplyr::pull(url) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "apis")
  # check synonymous functions call the above syntax
  # this is a parsimonious way to do checking, as it reduces API calls
  deparse(search_apis) |>
    stringr::str_detect("search_all\\(\"apis\"") |>
    any() |>
    expect_true()
})

test_that("`search_atlases()` returns a filtered result", {
  search_string <- "guat"
  result <- search_all(atlases, search_string)
  result |>
    dplyr::pull(region) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "atlases")
  # check synonymous function calls the above syntax
  deparse(search_atlases) |>
    stringr::str_detect("search_all\\(\"atlases\"") |>
    any() |>
    expect_true()
})

test_that("`search_collections()` returns a filtered result", {
  skip_galah()
  search_string <- "dna"
  result <- search_all(collections, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "collections")
  # check synonymous function calls the above syntax
  deparse(search_collections) |>
    stringr::str_detect("search_all\\(\"collections\"") |>
    any() |>
    expect_true()
})

test_that("`search_datasets()` returns a filtered result", {
  skip_galah()
  search_string <- "endangered"
  result <- search_all(datasets, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "datasets")
  # check synonymous function calls the above syntax
  deparse(search_datasets) |>
    stringr::str_detect("search_all\\(\"datasets\"") |>
    any() |>
    expect_true()
})

test_that("`search_fields()` returns a filtered result", {
  skip_galah()
  search_string <- "precipitation"
  result <- search_all(fields, search_string)
  result |>
    dplyr::pull(description) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "fields")
  # check synonymous function calls the above syntax
  deparse(search_fields) |>
    stringr::str_detect("search_all\\(\"fields\"") |>
    any() |>
    expect_true()
})

test_that("`search_fields()` helpful warning with blank argument", {
  skip_galah()
  expect_error(search_fields(), "We didn't detect a search")
})

test_that("`search_licenses()` returns a filtered result", {
  skip_galah()
  search_string <- "3.0"
  result <- search_all(licences, search_string)
  result |>
    dplyr::pull(acronym) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "licences")
  # check synonymous function calls the above syntax
  deparse(search_licences) |>
    stringr::str_detect("search_all\\(\"licences\"") |>
    any() |>
    expect_true()
})

# implement a smaller test for search_lists() to make tests faster
# this relies on a 'fake' query to `show_all_lists()` for testing purposes
# this is bad practice in production, but useful here
test_that("`search_lists()` returns a filtered result", {
  skip_galah()

  # manually override cached lists with something (much) smaller,
  # but still contains the term 'threatened'.
  galah_config(caching = FALSE)
  x <- request_metadata("lists") |>
    slice_head(n = 10) |>
    collapse()
  x$url <- "https://api.ala.org.au/specieslist/v2/speciesList?isAuthoritative=true&pageSize=10"
  y <- collect(x)
  galah_config(caching = TRUE)
  update_cache(lists = y) 

  # now resume earlier code
  search_string <- "threatened"
  quiet_lists_search <- purrr::quietly(search_lists)
  result <- quiet_lists_search(search_string) |>
    purrr::pluck("result")
  # elaborate check for matching terms in any character column
  chr_lookup <- purrr::map(colnames(result), 
                           is.character) |>
    unlist()
  chr_cols <- colnames(result)[chr_lookup]
  contains_term <- purrr::map(chr_cols, 
                              \(a){
                                result[[a]] |>
                                  tolower() |>
                                  stringr::str_detect("threatened")
  })
  purrr::map(purrr::list_transpose(contains_term), any) |>
    unlist() |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "lists")
  # check synonymous function calls the above syntax
  deparse(search_lists) |>
    stringr::str_detect("search_all\\(\"lists\"") |>
    any() |>
    expect_true()  
})

test_that("`search_reasons()` returns a filtered result", {
  skip_galah()
  search_string <- "sci"
  result <- search_all(reasons, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "reasons")
  # check synonymous function calls the above syntax
  deparse(search_reasons) |>
    stringr::str_detect("search_all\\(\"reasons\"") |>
    any() |>
    expect_true()
})

test_that("`search_ranks()` returns a filtered result", {
  search_string <- "kingdom"
  result <- search_all(ranks, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "ranks")
  # check synonymous function calls the above syntax
  deparse(search_ranks) |>
    stringr::str_detect("search_all\\(\"ranks\"") |>
    any() |>
    expect_true()
})

test_that("`search_profiles()` returns a filtered result", {
  skip_galah()
  search_string <- "base"
  result <- search_all(profiles, search_string)
  result |>
    dplyr::pull(description) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "profiles")
  # check synonymous function calls the above syntax
  deparse(search_profiles) |>
    stringr::str_detect("search_all\\(\"profiles\"") |>
    any() |>
    expect_true()
})

test_that("`search_providers()` returns a filtered result", {
  skip_galah()
  search_string <- "inaturalist"
  result <- search_all(providers, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_tibble(result)
  expect_equal(attributes(result)$call, "providers")
  # check synonymous function calls the above syntax
  deparse(search_providers) |>
    stringr::str_detect("search_all\\(\"providers\"") |>
    any() |>
    expect_true()
})
