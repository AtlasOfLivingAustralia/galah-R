# script to test whether `everything()` is respected (present or absent)
# in metadata requests. Caching is an important consideration here.

purrr_collect <- purrr::quietly(collect)
quiet_collect <- function(...){
  purrr_collect(...) |>
    purrr::pluck("result")
}

test_that("`request_metadata()` works with `select()` for local APIs", {
  type_list <- c("atlases",
                 "apis",
                 "ranks")
  x <- purrr::map(type_list, \(a){
    
    # build a query
    query <- request_metadata(type = a) |>
      collapse()
    
    # check this requests data
    query |>
      purrr::pluck("data") |>
      is.null() |>
      expect_false()
    # check `select` exists, and contains a quosure and a summary
    purrr::pluck(query, "select") |>
      is.null() |>
      expect_false()
    purrr::pluck(query, !!!list("select", 1)) |>
      rlang::is_quosure() |>
      expect_true()
    purrr::pluck(query, "select", "summary") |>
      is.null() |>
      expect_false()
    
    # collect result
    result <- collect(query)
    result |>
      inherits(c("tbl_df", "tbl", "data.frame")) |>
      expect_true()
    result |>
      nrow() |>
      expect_gt(0)
    
    # `select()` first two columns only
    first_2_cols <- colnames(result)[1:2]
    result2 <- request_metadata(type = a) |>
      select(tidyselect::any_of(first_2_cols)) |>
      collect()
    
    # check has worked
    expect_equal(colnames(result2),
                 first_2_cols)
  })
})

test_that("`request_metadata()` works with `select()` for remote APIs *without* default columns", {
  skip_if_offline()
  type_list <- c("collections",
                 "datasets",
                 "providers")
  reset_cache()
  
  x <- purrr::map(type_list, \(a){
    
    # build a query
    query <- request_metadata(type = a) |>
      collapse()
    
    # check this requests data
    query |>
      purrr::pluck("url") |>
      is.null() |>
      expect_false()
    # check `select` exists, and contains a quosure and a summary
    purrr::pluck(query, "select") |>
      is.null() |>
      expect_false()
    purrr::pluck(query, !!!list("select", 1)) |>
      rlang::is_quosure() |>
      expect_true()
    purrr::pluck(query, "select", "summary") |>
      is.null() |>
      expect_false()
    
    # collect result
    result <- collect(query)
    result |>
      inherits(c("tbl_df", "tbl", "data.frame")) |>
      expect_true()
    result |>
      nrow() |>
      expect_gt(0)
    
    # `select()` first two columns only
    first_2_cols <- colnames(result)[1:2]
    result2 <- request_metadata(type = a) |>
      select(tidyselect::any_of(first_2_cols)) |>
      collect()
    
    # check has worked
    expect_equal(colnames(result2),
                 first_2_cols)
  })
})

test_that("`request_metadata()` works with `select()` for remote APIs *with* default columns", {
  skip_if_offline()
  type_list <- c("assertions",
                 "fields",
                 "licences",
                 "lists",
                 "profiles",
                 "reasons")
  reset_cache()
  
  # run in a loop to check for all types
  # note `x` is captured to silence output, not because we need the results
  x <- purrr::map(type_list, \(a){
    
    # setup
    expected_columns <- wanted_columns(a)
    expected_n <- length(expected_columns)
    
    # set up a query _without_ `everything()`
    query <- request_metadata(type = a) |>
      collapse()
    # check this requests a url
    query |>
      purrr::pluck("url") |>
      is.null() |>
      expect_false()
    # check `select` exists, and contains a quosure and a summary
    purrr::pluck(query, "select") |>
      is.null() |>
      expect_false()
    purrr::pluck(query, !!!list("select", 1)) |>
      rlang::is_quosure() |>
      expect_true()
    purrr::pluck(query, "select", "summary") |>
      is.null() |>
      expect_false()
    
    # collect that query, and check for expected columns
    result <- quiet_collect(query)
    result |>
      ncol() |>
      expect_equal(expected_n)
    result |>
      colnames() |>
      expect_equal(expected_columns)
    
    # ensure results from the API are cached
    result_cached <- retrieve_cache(a)
    # because `everything()` was not called, cached tibble will have more cols
    # than returned tibble
    expect_gt(ncol(result_cached), ncol(result))
    
    # now construct a query _with_ `everything()`
    query_everything <- request_metadata(type = a) |>
      select(everything()) |>
      collapse()
    # because the full query is always cached, this should not include an API call
    query_everything |>
      purrr::pluck("url") |>
      is.null() |>
      expect_true()
    result_everything <- quiet_collect(query_everything)
    result_everything |>
      ncol() |>
      expect_gt(expected_n)
    result_everything |>
      colnames() |>
      expect_contains(expected_columns)
    result_cached <- retrieve_cache(a)
    expect_equal(colnames(result_everything), 
                 colnames(result_cached))
    expect_equal(nrow(result_everything), 
                 nrow(result_cached))
    
    # clean up
    reset_cache()
    return(a) # probably pointless, but neater than returning nothing
  })
})

test_that("`request_metdata()` works with `select()` for `type = 'taxa'`", {
  query <- request_metadata() |>
    identify("Crinia") |>
    select(everything()) |>
    collapse()
  # check this requests a url
  query |>
    purrr::pluck("url") |>
    is.null() |>
    expect_false()
  # check `select` exists, and contains a quosure and a summary
  purrr::pluck(query, "select") |>
    is.null() |>
    expect_false()
  purrr::pluck(query, !!!list("select", 1)) |>
    rlang::is_quosure() |>
    expect_true()
  purrr::pluck(query, "select", "summary") |>
    is.null() |>
    expect_false()
  # now run the query
  basic_search <- search_taxa("Crinia")
  everything_search <- quiet_collect(query)
  expect_gt(ncol(everything_search),
            ncol(basic_search))
  expect_equal(nrow(everything_search), 1)
  # look for some 'unusual' columns in the result
  expect_contains(colnames(everything_search),
                  c("lft", "rgt", "species_group", "genus_id"))
})

test_that("`request_metdata()` works with `select()` for complex taxa", {
  crinia_tibble <- tibble::tibble(kingdom = "Animalia",
                                  phylum = "Chordata",
                                  genus = "Crinia")
  query <- request_metadata() |>
    identify(crinia_tibble) |>
    select(everything()) |>
    collapse()
  # check this requests a url
  query |>
    purrr::pluck("url") |>
    is.null() |>
    expect_false()
  # check `select` exists, and contains a quosure and a summary
  purrr::pluck(query, "select") |>
    is.null() |>
    expect_false()
  purrr::pluck(query, !!!list("select", 1)) |>
    rlang::is_quosure() |>
    expect_true()
  purrr::pluck(query, "select", "summary") |>
    is.null() |>
    expect_false()
  # now run the query
  basic_search <- search_taxa(crinia_tibble)
  everything_search <- quiet_collect(query)
  expect_gt(ncol(everything_search),
            ncol(basic_search))
  expect_equal(nrow(everything_search), 1)
  # look for some 'unusual' columns in the result
  expect_contains(colnames(everything_search),
                  c("lft", "rgt", "species_group", "genus_id"))
})

test_that("`request_metdata()` works with `select()` for `type = 'identifiers'`", {
  tcid <- search_taxa("Chordata") |>
    dplyr::pull("taxon_concept_id")
  query <- request_metadata() |>
    filter(identifier == tcid) |>
    select(everything()) |>
    collapse()
  # check this requests a url
  query |>
    purrr::pluck("url") |>
    dplyr::pull("url") |>
    stringr::str_detect("namematching\\/api\\/getByTaxonID") |>
    expect_true()
  # check `select` exists, and contains a quosure and a summary
  purrr::pluck(query, "select") |>
    is.null() |>
    expect_false()
  purrr::pluck(query, !!!list("select", 1)) |>
    rlang::is_quosure() |>
    expect_true()
  purrr::pluck(query, "select", "summary") |>
    is.null() |>
    expect_false()
  # now run the query
  basic_search <- search_identifiers(tcid)
  everything_search <- quiet_collect(query)
  expect_gt(ncol(everything_search),
            ncol(basic_search))
  expect_equal(nrow(everything_search), 1)
  # look for some 'unusual' columns in the result
  expect_contains(colnames(everything_search),
                  c("success", "lft", "rgt", "kingdom_id"))
})

rm(purrr_collect, quiet_collect)