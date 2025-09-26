# quiet `show_values()`
purrr_values <- purrr::quietly(show_values)
quiet_values <- function(x, ...){
  purrr_values(x, ...) |>
    purrr::pluck("result")
}
# quiet `search_values()`
purrr_search <- purrr::quietly(search_values)
quiet_search <- function(x, y){
  purrr_search(x, query = y) |>
    purrr::pluck("result")
}

test_that("`show_values()` checks values", {
  skip_if_offline(); skip_on_ci()
  df <- tibble::tibble(x = c(1:2), y = c("a", "b"))
  wrong_type_search <- search_all(reasons, "sci")
  expect_error(show_values(), 'Missing information for values lookup.')
  expect_error(df |> show_values(), 'Wrong input provided')
  expect_error(wrong_type_search |> show_values(), "Can't lookup values for metadata type `reasons`.")
})

test_that("`show_values()` accepts search & show_all inputs from fields", {
  skip_if_offline(); skip_on_ci()
  # traditional syntax
  values_search <- search_all(lists, "EPBC act") |>
    quiet_values()
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  # newer syntax (doesn't require `show_all_lists()`)
  values_show <- request_metadata() |>
    filter(lists == "dr656") |>
    unnest() |>
    collect()
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("`show_values()` accepts search & show_all inputs from profiles", {
  skip_if_offline(); skip_on_ci()
  # traditional syntax
  values_search <- search_all(profiles, "ALA") |>
    quiet_values()
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  # newer syntax
  values_show <- request_metadata() |>
    filter(profiles == "ALA") |>
    unnest() |>
    collect()
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("`show_values()` accepts search & show_all inputs from lists", {
  skip_if_offline(); skip_on_ci()
  # old syntax
  values_search <- search_all(fields, "cl22") |>
    quiet_values()
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_search), 0)
  # new syntax
  values_show <- request_metadata() |>
    filter(fields == "basisOfRecord") |>
    unnest() |>
    collect()
  expect_s3_class(values_show, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(values_show), 0)
})

test_that("`search_values()` returns helpful error when missing query", {
  skip_if_offline(); skip_on_ci()
  expect_error(search_values(), "Missing information for values lookup")
  expect_error(search_all(fields, "cl22") |> purrr_search(), "didn't detect a search")
})

test_that("`search_values()` returns filtered results for fields", {
  skip_if_offline(); skip_on_ci()
  search <- search_all(fields, "cl22")
  values_search <- search |> quiet_search("new")
  values_show <- search |> quiet_values()
  search_result_check <- all(grepl(pattern = "new", 
                                   paste(values_search[,1]),
                                   ignore.case = TRUE))
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("`search_values()` returns filtered results for profiles", {
  skip_if_offline(); skip_on_ci()
  search <- search_all(profiles, "ALA")
  values_search <- search |> quiet_search("kingdom")
  values_show <- search |> quiet_values()
  search_result_check <- all(grepl(pattern = "kingdom", 
                                   paste(values_search$description),
                                   ignore.case = TRUE))
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("`search_values()` returns filtered results for lists", {
  skip_if_offline(); skip_on_ci()
  search <- search_all(lists, "ALA")
  values_search <- search |> quiet_search("frog")
  values_show <- search |> quiet_values()
  search_result_check <- all(grepl(pattern = "frog", 
                                   paste(values_search$commonName, values_search$scientificName),
                                   ignore.case = TRUE))
  expect_s3_class(values_search, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(values_search), names(values_show))
  expect_lt(nrow(values_search), nrow(values_show))
  expect_true(search_result_check)
})

test_that("`show_values()` & `search_values()` return number of matched fields", {
  skip_if_offline(); skip_on_ci()
  search1 <- search_fields("year")
  search2 <- search_fields("basisOfRecord")
  expect_message(search1 |> show_values(), "Showing values for 'year'")
  expect_message(search2 |> show_values(), "Showing values for 'basisOfRecord'")
})

test_that("`search_values()` specifies matched field", {
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

test_that("`show_values()` returns unformatted names", {
  skip_if_offline(); skip_on_ci()
  expected <- tibble::tibble(basisOfRecord = c("HUMAN_OBSERVATION",
                                               "PRESERVED_SPECIMEN"))
  search <- search_all(fields, "basisOfRecord")
  expect_equal(search |> quiet_values() |> head(2L), 
               expected)
})

test_that("`unnest()` syntax works", {
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

test_that("`show_values()` all_fields = TRUE works for lists", {
  skip_if_offline(); skip_on_ci()
  # simple, fake version for testing `show_values()`
  df <- tibble::tibble(species_list_uid = "dr650")
  attr(df, "call") <- "lists"
  show_values_query <- quiet_values(df, all_fields = TRUE)
  # NOTE: above is same as following code, but much faster  
  # search <- search_all(lists, "dr650") |>
  #   show_values(all_fields = TRUE)
  extra_cols <- c("raw_scientificName", "status", "sourceStatus", "IUCN_equivalent_status")
  expect_s3_class(show_values_query, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(show_values_query), 0)
  expect_true(any(colnames(show_values_query) %in% extra_cols))
  expect_gt(ncol(show_values_query), 6) # adds additional columns
  # doesn't work for fields
  x <- search_all(fields, "cl22") |>
    purrr_values(all_fields = TRUE)
  expect_equal(x$warnings,
               "`all_fields` only applies to type `lists`. Ignoring `all_fields = TRUE`.")
  expect_equal(x$messages,
               "* Showing values for 'cl22'.")
})

test_that("unnest() |> `select(everything()) works as alternative to all_fields",{
  x <- request_metadata() |>
    filter(list == "dr650") |>
    select(everything()) |>
    unnest() |>
    collect()
  extra_cols <- c("raw_scientificName", "status", "sourceStatus", "IUCN_equivalent_status")
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_gt(nrow(x), 0)
  expect_true(any(colnames(x) %in% extra_cols))
  expect_gt(ncol(x), 6) # adds additional columns
  
  # explicitly errors for other metadata types
  request_metadata() |>
    filter(field == "basisOfRecord") |>
    select(everything()) |>
    unnest() |>
    expect_error()
})

rm(purrr_values, quiet_values, purrr_search, quiet_search)