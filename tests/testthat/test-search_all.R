# NOTE: Tests are skipped on GH Actions using `skip_on_ci()` to avoid throttling 
#       API 
#       (these are probably not built for many fast queries if output is large)

test_that("search_all checks inputs, returns helpful error", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  expect_error(search_all(attributes, ""), "Unrecognised metadata requested")
})

test_that("search_all returns correct output for type", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  fields <- search_all(fields, "year")
  reasons <- search_all(reasons, "genus")
  profiles <- search_all(profiles, "ala")
  
  expect_s3_class(fields, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(reasons, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(profiles, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(fields, search_fields("year"))
  expect_equivalent(reasons, search_reasons("genus"))
  expect_equivalent(profiles, search_profiles("ala"))
  expect_equal(attributes(fields)$call, "fields")
  expect_equal(attributes(reasons)$call, "reasons")
  expect_equal(attributes(profiles)$call, "profiles")
  expect_equal(ncol(fields), 3)
  expect_equal(ncol(reasons), 2)
  expect_equal(ncol(profiles), 4)
})

test_that("search_all returns error when missing query", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  expect_error(search_all(profiles), "We didn't detect a search query")
  expect_error(search_all(fields, blah))
})

test_that("search_assertions returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
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

test_that("search_apis returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_apis()
  search <- search_apis("image")
  search2 <- search_all(apis, "image")
  search_result_check <- all(grepl(pattern = "image", search$url,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "apis")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_atlases returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_atlases()
  search <- search_atlases("guat")
  search2 <- search_all(atlases, "guat")
  search_result_check <- all(grepl(pattern = "guat", search$region,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "atlases")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_collections returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_collections()
  search <- search_collections("dna")
  search2 <- search_all(collections, "dna")
  search_result_check <- all(grepl(pattern = "dna", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "collections")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_datasets returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_datasets()
  search <- search_datasets("endangered")
  search2 <- search_all(datasets, "endangered")
  search_result_check <- all(grepl(pattern = "endangered", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "datasets")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_fields returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_fields()
  search <- search_fields("precipitation")
  search2 <- search_all(fields, "precipitation")
  search_result_check <- all(grepl(pattern = "precipitation", search$description,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "fields")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_fields helpful warning with blank argument", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  expect_error(search_fields(), "We didn't detect a search query.")
})

test_that("search_licenses returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_licences()
  search <- search_licences("3.0")
  search2 <- search_all(licences, "3.0")
  search_result_check <- all(grepl(pattern = "3.0", search$acronym,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "licences")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_lists returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_lists()
  search <- search_lists("threatened")
  search2 <- search_all(lists, "threatened")
  search_result_check <- all(grepl(pattern = "threatened", search$listName,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "lists")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_reasons returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_reasons()
  search <- search_reasons("sci")
  search2 <- search_all(reasons, "sci")
  search_result_check <- all(grepl(pattern = "sci", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "reasons")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_ranks returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_ranks()
  search <- search_ranks("kingdom")
  search2 <- search_all(ranks, "kingdom")
  search_result_check <- all(grepl(pattern = "kingdom", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "ranks")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_profiles returns a filtered result", {
  skip_on_cran(); skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_profiles()
  search <- search_profiles("base")
  search2 <- search_all(profiles, "base")
  search_result_check <- all(grepl(pattern = "base", search$description,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "profiles")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})

test_that("search_providers returns a filtered result", {
  skip_on_cran(); skip_if_offline(); skip_on_ci()
  all <- show_all_providers()
  search <- search_providers("inaturalist")
  search2 <- search_all(providers, "inaturalist")
  search_result_check <- all(grepl(pattern = "inaturalist", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "providers")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
  expect_equal(search, search2)
})
