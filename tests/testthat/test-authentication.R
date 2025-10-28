quiet_config <- purrr::quietly(galah_config)

test_that("`galah_config()` caches config info when `authenticate` is set to `TRUE`", {
  skip_on_ci(); skip_on_cran()
  x <- galah_config()
  expect_false(x$package$authenticate)
  expect_true(is.null(retrieve_cache("config")))
  y <- quiet_config(authenticate = TRUE)
  stringr::str_detect(y$messages, 
                      "Caching `config` information to support authentication") |>
    any() |>
    expect_true()
  expect_true(y$result$package$authenticate)
  cached_config <- retrieve_cache("config")
  expect_false(is.null(cached_config))
  expect_equal(nrow(cached_config), 1)
})


test_that("`request_metadata()` works for type = `config`", {
  skip_on_ci(); skip_on_cran()
  result <- request_metadata(type = "config") |>
    collect()
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 4)
  expect_true(all(
    c("client_id", "authorize_url", "token_url", "scopes") %in% 
    colnames(result)))
})

test_that("`request_metadata()` caches type `config` correctly", {
  skip_on_ci(); skip_on_cran()
  reset_cache()
  result <- request_metadata(type = "config") |>
    as_query() 
  expect_true(!is.null(result$data))  
})

test_that("setting `authentication` to `TRUE` doesn't break a query", {
  skip("Authentication currently requires user interaction")
  config <- quiet_config(authenticate = TRUE)
  result <- request_metadata(type = "reasons") |>
    collect() |>
    expect_no_error()
  result |>
    inherits(c("tbl_df", "tbl", "data.frame")) |>
    expect_true()
  expect_gt(nrow(result), 1)
  expect_equal(ncol(result), 2)
  # reset
  galah_config(authentication = FALSE)
})

test_that("setting `authentication` to `TRUE` changes data returned", {
  skip_on_ci(); skip_on_cran()
  skip_if(!file.exists(".secure-credentials"),
          "Secret information not provided")
  
  # load credentials
  do.call(galah_config,
          jsonlite::fromJSON(".secure-credentials"))
  # add other options
  config <- quiet_config(directory = "TEST-SENSITIVE-DATA",
                         authenticate = TRUE,
                         run_checks = FALSE)

  # These credentials *should* give access to sensitive data for Tasmania *only*
  # subset to species on Tasmania's sensitive species list
  result <- galah_call() |>
    filter(species_list_uid == "dr491") |>
    collect()
  
  # check sensitive columns exist
  colnames(result) |>
    stringr::str_detect("^sensitive_") |>
    any() |>
    expect_true()
  
  # expect some sensitive spatial data to not be NA
  na_values <- result |>
    dplyr::pull(sensitive_decimalLatitude) |>
    is.na() 
  any(!na_values) |>
    expect_true()
  
  # expect higher precision in sensitive columns, where any data given
  precise_latitude <- result$sensitive_decimalLatitude[!na_values] |>
    as.character() |>
    nchar()
  imprecise_latitude <- result$decimalLatitude[!na_values] |>
    as.character() |>
    nchar()
  imprecise_latitude[is.na(imprecise_latitude)] <- 0
  # test
  all(precise_latitude >= imprecise_latitude) |>
    expect_true()

  unlink("TEST-SENSITIVE-DATA", recursive = TRUE)
  config <- quiet_config(authenticate = FALSE,
                         directory = "TESTING")
})

rm(quiet_config)