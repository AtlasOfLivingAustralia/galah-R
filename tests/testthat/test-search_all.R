test_that("search_all checks inputs, returns helpful error", {
  skip_if_offline(); skip_on_ci()
  expect_error(search_all(attributes, ""), "Unrecognised metadata requested")
})

test_that("search_all returns correct output for type", {
  skip_if_offline(); skip_on_ci()
  fields <- search_all(fields, "year")
  reasons <- search_all(reasons, "genus")
  profiles <- search_all(profiles, "ala")
  
  expect_s3_class(fields, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(reasons, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(profiles, c("tbl_df", "tbl", "data.frame"))
  expect_equal(fields, search_fields("year"))
  expect_equal(reasons, search_reasons("genus"))
  expect_equal(profiles, search_profiles("ala"))
  expect_equal(attributes(fields)$call, "fields")
  expect_equal(attributes(reasons)$call, "reasons")
  expect_equal(attributes(profiles)$call, "profiles")
  expect_equal(ncol(fields), 3)
  expect_equal(ncol(reasons), 2)
  expect_equal(ncol(profiles), 4)
})

test_that("search_all returns error when missing query", {
  skip_if_offline(); skip_on_ci()
  expect_error(search_all(profiles), "We didn't detect a search")
  expect_error(search_all(fields, blah))
})

test_that("search_assertions returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  all <- show_all_assertions()
  search <- search_assertions("INVALID")
  search2 <- search_all(assertions, "INVALID")
  search_result_check <- all(grepl(pattern = "invalid", 
                                   paste(search$description, search$id),
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "assertions")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
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
  expect_equal(attributes(result)$call, "apis")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
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
  expect_equal(attributes(result)$call, "atlases")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_atlases) |>
    stringr::str_detect("search_all\\(\"atlases\"") |>
    any() |>
    expect_true()
})

test_that("`search_collections()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "dna"
  result <- search_all(collections, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "collections")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_collections) |>
    stringr::str_detect("search_all\\(\"collections\"") |>
    any() |>
    expect_true()
})

test_that("`search_datasets()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "endangered"
  result <- search_all(datasets, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "datasets")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_datasets) |>
    stringr::str_detect("search_all\\(\"datasets\"") |>
    any() |>
    expect_true()
})

test_that("`search_fields()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "precipitation"
  result <- search_all(fields, search_string)
  result |>
    dplyr::pull(description) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "fields")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_fields) |>
    stringr::str_detect("search_all\\(\"fields\"") |>
    any() |>
    expect_true()
})

test_that("`search_fields()` helpful warning with blank argument", {
  skip_if_offline(); skip_on_ci()
  expect_error(search_fields(), "We didn't detect a search")
})

test_that("`search_licenses()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "3.0"
  result <- search_all(licences, search_string)
  result |>
    dplyr::pull(acronym) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "licences")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_licences) |>
    stringr::str_detect("search_all\\(\"licences\"") |>
    any() |>
    expect_true()
})

test_that("`search_lists()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
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
  expect_equal(attributes(result)$call, "lists")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_lists) |>
    stringr::str_detect("search_all\\(\"lists\"") |>
    any() |>
    expect_true()  
})

test_that("`search_reasons()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "sci"
  result <- search_all(reasons, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "reasons")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
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
  expect_equal(attributes(result)$call, "ranks")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_ranks) |>
    stringr::str_detect("search_all\\(\"ranks\"") |>
    any() |>
    expect_true()
})

test_that("`search_profiles()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "base"
  result <- search_all(profiles, search_string)
  result |>
    dplyr::pull(description) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "profiles")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_profiles) |>
    stringr::str_detect("search_all\\(\"profiles\"") |>
    any() |>
    expect_true()
})

test_that("`search_providers()` returns a filtered result", {
  skip_if_offline(); skip_on_ci()
  search_string <- "inaturalist"
  result <- search_all(providers, search_string)
  result |>
    dplyr::pull(name) |>
    tolower() |>
    stringr::str_detect(search_string) |>
    all() |>
    expect_true()
  expect_equal(attributes(result)$call, "providers")
  expect_s3_class(result, c("tbl_df", "tbl", "data.frame"))
  # check synonymous function calls the above syntax
  deparse(search_providers) |>
    stringr::str_detect("search_all\\(\"providers\"") |>
    any() |>
    expect_true()
})
