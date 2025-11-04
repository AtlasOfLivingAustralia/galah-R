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

test_that("`use_authentication()` works in-pipe for metadata", {
  query <- request_metadata(type = "reasons") |>
    use_authentication()
  
  result <- as_query(query)
  is.null(result$authenticate) |>
    expect_false()
  
  result2 <- coalesce(result)
  expect_equal(length(result2), 2)

  # NOTE: this shows both datasets are set to `data` (not `url`)
  # so `use_authentication()` won't do anything
  # perhaps a solution is to have a `force` argument to ensure query happens
  # this would be logical to put in `collect()` and `show_all()`;
  # but would be evaluated in `collapse()` so would go there too
  
  # in which case, setting `use_authentication()` should set `force = TRUE`
  
  # NOTE: `use_credentials()` could be a good counterpoint for setting email etc
})

test_that("`use_authentication()` works in-pipe for occurrences", {
  galah_config(email = "ala4r@ala.org.au")
  
  query <- galah_call() |>
    identify("Litoria dentata") |>
    filter(year == 2025) |>
    use_authentication() |>
    coalesce()
  expect_equal(length(query), 6)
  is.null(query[[6]]$authenticate) |>
    expect_false()
  
  x <- collapse(query) # FIXME: errors with no email address found
  # note that this shouldn't happen if authentication has worked;
  # BUT we haven't tested that yet
  x |>
    purrr::pluck("authenticate") |>
    is.null() |>
    expect_false()
  
  y <- compute(x)
  # failing here
  
  # once auth works, this should still contain authentication metadata
  
  z <- collect(y)
  
})

test_that("setting `authentication` to `TRUE` changes data returned", {
  skip_on_ci(); skip_on_cran()
  skip_if(!file.exists(".secure-credentials"),
          "Secret information not provided")
  
  # load credentials, set authenticate to TRIE
  config <- c(
    jsonlite::fromJSON(".secure-credentials"),
    list(directory = "TEST-SENSITIVE-DATA",
         authenticate = TRUE)) |>
    quiet_config()

  # These credentials *should* give access to sensitive data for Tasmania *only*
  # subset to species on Tasmania's sensitive species list
  result <- galah_call() |>
    filter(species_list_uid == "dr491") |>
    collect() |>
    expect_no_error() # check for exception to `check_field_identities()`
  
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

# Downloading from a DOI fails
# galah_call() |>
#     filter(doi == "ala.3d0e08ac-d0ec-420d-a1f7-8cde778e82f6") |>
#     collect()
# May be same problem as previously documented

rm(quiet_config)
