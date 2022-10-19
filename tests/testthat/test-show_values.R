context("Test show_values & search_values")

test_that("show_values checks values", {
  skip_on_cran()
  df <- tibble::tibble(x = c(1:2), y = c("a", "b"))
  wrong_type_search <- search_all(reasons, "sci")
  
  expect_error(show_values(), 'No input detected')
  expect_error(df |> show_values(), 'Wrong input provided')
  expect_error(wrong_type_search |> show_values(), "Unsupported 'type'")
})

test_that("show_values accepts search & show_all inputs from fields", {
  skip_on_cran()
  search <- search_all(lists, "EPBCact1")
  filtered_show <- show_all(lists) |>
    dplyr::filter(dataResourceUid == "dr656")
  values_search <- search |> show_values()
  values_show <- filtered_show |> show_values()
  
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("show_values accepts search & show_all inputs from profiles", {
  skip_on_cran()
  search <- search_all(profiles, "ALA")
  filtered_show <- show_all(profiles) |>
    dplyr::filter(shortName == "ALA")
  values_search <- search |> show_values()
  values_show <- filtered_show |> show_values()
  
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("show_values accepts search & show_all inputs from lists", {
  skip_on_cran()
  search <- search_all(fields, "cl22")
  filtered_show <- show_all(fields) |>
    dplyr::filter(id == "year")
  values_search <- search |> show_values()
  values_show <- filtered_show |> show_values()
  
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("search_values returns helpful error when missing query", {
  skip_on_cran()
  expect_error(search_values(), "No input detected")
  expect_error(search_all(fields, "cl22") |> search_values(), "didn't detect a valid query")
})

test_that("search_values returns filtered results for fields", {
  skip_on_cran()
  search <- search_all(fields, "cl22")
  values_search <- search |> search_values("new")
  values_show <- search |> show_values()
  search_result_check <- all(grepl(pattern = "new", 
                                   paste(values_search$category),
                                   ignore.case = TRUE))

  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("search_values returns filtered results for profiles", {
  skip_on_cran()
  search <- search_all(profiles, "ALA")
  values_search <- search |> search_values("kingdom")
  values_show <- search |> show_values()
  search_result_check <- all(grepl(pattern = "kingdom", 
                                   paste(values_search$description),
                                   ignore.case = TRUE))
  
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("search_values returns filtered results for profiles", {
  skip_on_cran()
  search <- search_all(lists, "ALA")
  values_search <- search |> search_values("frog")
  values_show <- search |> show_values()
  search_result_check <- all(grepl(pattern = "frog", 
                                   paste(values_search$commonName, values_search$scientificName),
                                   ignore.case = TRUE))
  
  
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("show_values & search_values return number of matched fields", {
  skip_on_cran()
  search1 <- search_fields("year")
  search2 <- search_fields("basisOfRecord")
  
  expect_message(search1 |> show_values(), "Showing values for 'year'")
  expect_message(search2 |> show_values(), "Showing values for 'basisOfRecord'")
})

test_that("search_values specifies matched field", {
  skip_on_cran()
  search1 <- search_fields("year")
  search2 <- search_fields("state")
  search3 <- search_profiles("ALA")
  n_fields1 <- paste(nrow(search1))
  n_fields2 <- paste(nrow(search2))
  n_fields3 <- paste(nrow(search3))
  
  expect_message(search1 |> show_values(), n_fields1)
  expect_message(search2 |> show_values(), n_fields2)
  expect_message(search3 |> show_values(), n_fields3)
})

test_that("show_values and search_values work for collections", {
  skip_on_cran()
  result <- expect_message({
    show_all_collections() |>
    show_values()
  })
  expect_equal(nrow(result), 1)
  result <- expect_message({
    search_collections("co43") |> 
    show_values()
  })
  expect_equal(nrow(result), 1)
  result <- expect_warning({
    show_all_collections() |>
    search_values("something")
  })
  expect_equal(nrow(result), 1)
})

test_that("show_values and search_values work for datasets", {
  skip_on_cran()
  result <- expect_message({
    show_all_datasets() |>
    show_values()
  })
  expect_equal(nrow(result), 1)
  result <- expect_message({
    search_datasets("dr14507") |> 
    show_values()
  })
  expect_equal(nrow(result), 1)
  result <- expect_warning({
    show_all_datasets() |>
    search_values("something")
  })
  expect_equal(nrow(result), 1)
})

test_that("show_values and search_values work for providers", {
  skip_on_cran()
  result <- expect_message({
    show_all_providers() |>
    show_values()
  })
  expect_equal(nrow(result), 1)
  result <- expect_message({
    search_providers("dp1977") |> 
    show_values()
  })
  expect_equal(nrow(result), 1)
  result <- expect_warning({
    show_all_providers() |>
    search_values("something")
  })
  expect_equal(nrow(result), 1)
})