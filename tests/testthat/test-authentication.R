quiet_config <- purrr::quietly(galah_config)

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
  x <- request_metadata(type = "config") |>
    collect()
  result <- request_metadata(type = "config") |>
    as_query() 
  expect_true(!is.null(result$data))  
})

test_that("`use_authentication()` works in-pipe for metadata", {
  skip("authentication requires interactivity")
  galah_config(caching = FALSE) # turn off caching to force galah to call an API
  query <- request_metadata(type = "reasons") |>
    use_authentication()
  
  result <- as_query(query)
  is.null(result$authenticate) |>
    expect_false()
  
  result2 <- coalesce(result)
  expect_equal(length(result2), 2)

  galah_config(caching = TRUE)
})

test_that("`use_authentication()` works in-pipe for occurrences", {
  skip("authentication requires interactivity")
  
  galah_config(authenticate = TRUE)
  query <- galah_call() |>
    identify("Litoria dentata") |>
    filter(year == 2025) |>
    coalesce()
  expect_equal(length(query), 6)
  is.null(query[[6]]$authenticate) |>
    expect_false()
  
  x <- collapse(query)
  x |>
    purrr::pluck("authenticate") |>
    is.null() |>
    expect_false()
  
  y <- compute(x)
  inherits(y, "computed_query") |>
    expect_true()
  any(names(y) == "authenticate") |>
    expect_true()
  
  z <- collect(y)
  stringr::str_detect(names(z), "^sensitive_") |>
    any() |>
    expect_true()
})

test_that("setting `authentication` to `TRUE` changes data returned", {
  skip("authentication requires interactivity")
  
  # NOTE: credentials *should* give access to sensitive data for Tasmania *only*
  # subset to species on Tasmania's sensitive species list
  galah_config(authenticate = TRUE)
  
  # convert to query set first
  x_queryset <- galah_call() |>
    filter(species_list_uid == "dr491") |>
    coalesce()
  expect_equal(length(x_queryset), 5)
  expect_equal(x_queryset[[1]]$type, 
               "metadata/config")
  is.null(x_queryset[[5]]$authenticate) |>
    expect_false()
  # unclear whether it is _critical_ for coalesce() to source `show_all_config()` here
  # but some use cases it probably is necessary, and for the others it is 
  # 'free' because of caching, so probably best to leave it for now
  
  # then collapse
  x_query <- collapse(x_queryset)
  stringr::str_detect(x_query$url, "&email=") |>
    expect_false()
  # TODO add `authenticate` to `print.query()`
  
  # compute
  y <- compute(x_query) # note: triggers authentication a second time?!
  # check for messages
  
  result <- collect(y)  |>
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
#     use_authentication() |>
#     collect()
# May be same problem as previously documented

rm(quiet_config)
galah_config(caching = TRUE,
             authenticate = FALSE,
             email = "ala4r@ala.org.au")
