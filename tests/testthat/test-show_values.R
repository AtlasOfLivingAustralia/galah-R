test_that("show_values checks values", {
  skip_if_offline(); skip_on_ci()
  df <- tibble::tibble(x = c(1:2), y = c("a", "b"))
  wrong_type_search <- search_all(reasons, "sci")
  expect_error(show_values(), 'Missing information for values lookup.')
  expect_error(df |> show_values(), 'Wrong input provided')
  expect_error(wrong_type_search |> show_values(), "Can't lookup values for metadata type `reasons`.")
})

test_that("show_values accepts search & show_all inputs from fields", {
  skip_if_offline(); skip_on_ci()
  search <- search_all(lists, "EPBC act")
  filtered_show <- show_all(lists) |>
    dplyr::filter(species_list_uid == "dr656")
  values_search <- search |> show_values()
  values_show <- filtered_show |> show_values()
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("show_values accepts search & show_all inputs from profiles", {
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
  expect_error(search_values(), "Missing information for values lookup")
  expect_error(search_all(fields, "cl22") |> search_values(), "didn't detect a search query")
})

test_that("search_values returns filtered results for fields", {
  skip_if_offline(); skip_on_ci()
  search <- search_all(fields, "cl22")
  values_search <- search |> search_values("new")
  values_show <- search |> show_values()
  search_result_check <- all(grepl(pattern = "new", 
                                   paste(values_search[,1]),
                                   ignore.case = TRUE))
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equivalent(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("search_values returns filtered results for profiles", {
  skip_if_offline(); skip_on_ci()
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

test_that("search_values returns filtered results for lists", {
  skip_if_offline(); skip_on_ci()
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
  skip_if_offline(); skip_on_ci()
  search1 <- search_fields("year")
  search2 <- search_fields("basisOfRecord")
  expect_message(search1 |> show_values(), "Showing values for 'year'")
  expect_message(search2 |> show_values(), "Showing values for 'basisOfRecord'")
})

test_that("search_values specifies matched field", {
  skip_if_offline(); skip_on_ci()
  search1 <- search_fields("year")
  search2 <- search_fields("state")
  search3 <- search_profiles("ALA")
  n_fields1 <- paste(nrow(search1))
  n_fields2 <- paste(nrow(search2))
  n_fields3 <- paste(nrow(search3))
  expect_message(search1 |> show_values(), n_fields1)
  expect_message(search2 |> show_values(), n_fields2)
  # expect_message(search3 |> show_values(), n_fields3)
})

test_that("show_values returns unformatted names", {
  skip_if_offline(); skip_on_ci()
  expected <- tibble(basisOfRecord = c("HUMAN_OBSERVATION",
                                       "PRESERVED_SPECIMEN"))
  search <- search_all(fields, "basisOfRecord")
  expect_equal(search |> show_values() |> head(2L), 
               expected)
})

test_that("unnest syntax works", {
  skip_if_offline(); skip_on_ci()
  # fields
  x <- request_metadata() |>
    filter(field == "cl22") |>
    unnest() |>
    collect() 
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(x), "cl22")
  expect_gte(nrow(x), 1)
  # profiles
  y <- request_metadata() |>
    filter(profile == "ALA") |>
    unnest() |>
    collect() 
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_gte(ncol(y), 4)
  expect_gte(nrow(y), 1)
})
