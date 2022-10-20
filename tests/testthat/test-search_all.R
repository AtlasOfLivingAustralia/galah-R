context("Test search_fields")

test_that("search_all checks inputs, returns helpful error", {
  skip_on_cran()
  expect_error(search_all(attributes, ""), "is not recognised")
})

test_that("search_all returns correct output for type", {
  skip_on_cran()
  fields <- search_all(fields, "year")
  reasons <- search_all(reasons, "genus")
  profiles <- search_all(profiles, "ala")
  
  expect_s3_class(fields, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(reasons, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(profiles, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(fields, search_fields("year"))
  expect_equivalent(reasons, search_reasons("genus"))
  expect_equivalent(profiles, search_profiles("ala"))
  expect_equal(attributes(fields)$call, "search_fields")
  expect_equal(attributes(reasons)$call, "search_reasons")
  expect_equal(attributes(profiles)$call, "search_profiles")
  expect_equal(ncol(fields), 4)
  expect_equal(ncol(reasons), 2)
  expect_equal(ncol(profiles), 4)
})

test_that("search_all returns error when missing query", {
  skip_on_cran()
  expect_error(search_all(profiles), "didn't detect a valid query")
  expect_error(search_all(fields, blah))
})

test_that("search_assertions returns a filtered result", {
  skip_on_cran()
  all <- show_all_assertions()
  search <- search_assertions("INVALID")
  search_result_check <- all(grepl(pattern = "invalid", 
                                   paste(search$description, search$id),
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_assertions")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_apis returns a filtered result", {
  skip_on_cran()
  all <- show_all_apis()
  search <- search_apis("image")
  search_result_check <- all(grepl(pattern = "image", search$api_name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_apis")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_atlases returns a filtered result", {
  skip_on_cran()
  all <- show_all_atlases()
  search <- search_atlases("guat")
  search_result_check <- all(grepl(pattern = "guat", search$atlas,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_atlases")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_collections returns a filtered result", {
  skip_on_cran()
  all <- show_all_collections()
  search <- search_collections("dna")
  search_result_check <- all(grepl(pattern = "dna", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_collections")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_datasets returns a filtered result", {
  skip_on_cran()
  all <- show_all_datasets()
  search <- search_datasets("endangered")
  search_result_check <- all(grepl(pattern = "endangered", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_datasets")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_providers returns a filtered result", {
  skip_on_cran()
  all <- show_all_providers()
  search <- search_providers("inaturalist")
  search_result_check <- all(grepl(pattern = "inaturalist", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_providers")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})


test_that("search_fields returns a filtered result", {
  skip_on_cran()
  all <- show_all_fields()
  search <- search_fields("precipitation")
  search_result_check <- all(grepl(pattern = "precipitation", search$description,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_fields")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_fields helpful warning with blank argument", {
  skip_on_cran()
  expect_warning(search_fields(), "didn't detect a field")
})


test_that("search_licenses returns a filtered result", {
  skip_on_cran()
  all <- show_all_licences()
  search <- search_licences("3.0")
  search_result_check <- all(grepl(pattern = "3.0", search$acronym,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_licences")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_reasons returns a filtered result", {
  skip_on_cran()
  all <- show_all_reasons()
  search <- search_reasons("sci")
  search_result_check <- all(grepl(pattern = "sci", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_reasons")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_ranks returns a filtered result", {
  skip_on_cran()
  all <- show_all_ranks()
  search <- search_ranks("kingdom")
  search_result_check <- all(grepl(pattern = "kingdom", search$name,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_ranks")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})


test_that("search_profiles returns a filtered result", {
  skip_on_cran()
  all <- show_all_profiles()
  search <- search_profiles("base")
  search_result_check <- all(grepl(pattern = "base", search$description,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_profiles")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})

test_that("search_lists returns a filtered result", {
  skip_on_cran()
  all <- show_all_lists()
  search <- search_lists("threatened")
  search_result_check <- all(grepl(pattern = "threatened", search$listName,
                                   ignore.case = TRUE))
  
  expect_lt(nrow(search), nrow(all))
  expect_equal(attributes(search)$call, "search_lists")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_true(search_result_check)
})
