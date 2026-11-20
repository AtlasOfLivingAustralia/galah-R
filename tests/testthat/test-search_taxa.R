purrr_taxa <- purrr::quietly(search_taxa)
purrr_identifiers <- purrr::quietly(search_identifiers)

test_that("`search_taxa()` checks inputs", {
  expect_error(search_taxa())
})

test_that("`search_taxa()` works for simple queries", {
  skip_galah()
  x <- search_taxa("Microseris lanceolata")
  expect_tibble(x)
  expect_equal(nrow(x), 1)
})

test_that("`search_taxa()` works for multiple queries", {
  skip_galah()
  x <- search_taxa(c("Eucalyptus", "Banksia", "Acacia"))
  expect_tibble(x)
  expect_equal(nrow(x), 3)
})

test_that("`search_taxa()` handles data.frame input", {
  skip_galah()
  test_df <- data.frame(genus = c("Banksia", "Microseris"),
                        kingdom = "Plantae")
  x <- request_metadata() |>
    identify(test_df) |>
    collect()
  y <- search_taxa(test_df)
  expect_tibble(x)
  expect_true(identical(x, y))
  expect_equal(nrow(y), 2)
})

test_that("`search_taxa()` errors when no columns match a taxon rank", {
  skip_galah()
  test_df <- tibble::tibble(c("Banksia", "Microseris"),
                            "Plantae")
  search_taxa(test_df) |>
    expect_error()
})

test_that("`search_taxa()` warns when some columns don't match a taxon rank", {
  skip_galah()
  test_df <- tibble::tibble(genus = c("Banksia", "Microseris"),
                            kingdoom = "Plantae")
  search_taxa(test_df) |>
    expect_warning()
})

test_that("`search_taxa()` gives a message for invalid names", {
  skip_galah()
  galah_config(verbose = TRUE)
  x <- purrr_taxa("bad_term")
  x |>
    purrr::pluck("messages") |>
    stringr::str_detect("^Matched 0 of 1 taxonomic search terms") |>
    any() |>
    expect_true()
  galah_config(verbose = FALSE)
})

test_that("`search_taxa()` handles a mix of valid and invalid queries", {
  skip_galah()
  galah_config(verbose = TRUE)
  x <- c("Eucalyptus", "Banksia", "Wattle", "wootle") |>
    purrr_taxa()
  x |>
    purrr::pluck("messages") |>
    stringr::str_detect("^Matched 3 of 4 taxonomic search terms") |>
    any() |>
    expect_true()
  x |>
    purrr::pluck("result") |>
    nrow() |>
    expect_equal(4)
  galah_config(verbose = FALSE)
})

test_that("`search_taxa()` searches using multiple ranks", {
  skip_galah()
  x <- data.frame(genus = "Acacia", 
                     kingdom = "Plantae") |>
    search_taxa()
  expect_tibble(x)
  expect_equal(x$rank, "genus")
  expect_equal(nrow(x), 1)
})

test_that("`search_taxa()` gives an error when homonyms are returned", {
  skip_galah()
  expect_warning(search_taxa("ACANTHOCEPHALA"))
})

test_that("`search_taxa()` handles name issues", {
  skip_galah()
  expect_warning(search_taxa("Microseris"))
})

test_that("`search_taxa()` give an error when homonym is returned with other issues", {
  skip_galah()
  expect_warning(search_taxa("Gallinago sp."))
})

test_that("`search_taxa()` errors nicely when piped in `galah_call()`", {
  expect_error(galah_call() |> search_taxa("perameles"), "Can't pipe `search_taxa()")
})

test_that("`search_taxa()` handles complex strings", {
  skip_galah()
  x <- search_taxa("Commelina sp. (Lakefield NP P.I.Forster+ PIF12943)")
  expect_tibble(x)
  expect_equal(x$scientific_name, 
               "Commelina sp. Lakefield NP (P.I.Forster+ PIF12943)")
  expect_equal(nrow(x), 1)
})

test_that("`search_identifiers()` searches using identifier", {
  skip_galah()
  # check different types of id
  identifier <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
                  "NZOR-6-1742", "https://id.biodiversity.org.au/node/apni/2910467")
  x <- search_identifiers(identifier)
  expect_tibble(x)
  expect_equal(nrow(x), 3)
})

test_that("`search_identifiers()` gives a message for invalid ids", {
  skip_galah()
  galah_config(verbose = TRUE)
  x <- purrr_identifiers("1234")
  x |>
    purrr::pluck("messages") |>
    stringr::str_detect("^Matched 0 of 1 taxonomic search terms") |>
    any() |>
    expect_true()
  galah_config(verbose = FALSE)
})

test_that("`search_identifiers()` works when one value is missing", {
  skip_galah()
  # check different types of id
  identifier <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
                  "something")
  galah_config(verbose = TRUE)
  x <- purrr_identifiers(identifier)
  x |>
    purrr::pluck("messages") |>
    stringr::str_detect("^Matched 1 of 2 taxonomic search terms") |>
    any() |>
    expect_true()
  x |>
    purrr::pluck("result") |>
    expect_tibble()
  x |>
    purrr::pluck("result") |>
    nrow() |>
    expect_equal(2)
  galah_config(verbose = FALSE)
})

test_that("`search_identifiers()` works via `search_all()`", {
  skip_galah()
  id <- "urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed"
  search <- search_identifiers(id)
  search2 <- search_all(identifiers, id)
  expect_tibble(search)
  expect_equal(nrow(search), 1)
  expect_equal(search, search2)
})

test_that("`request_metadata()` works for `type = 'taxa'`", {
  skip_galah()
  x <- request_metadata() |>
    identify("crinia") 
  expect_s3_class(x, "metadata_request")
  expect_equal(names(x), c("type", "atlas", "identify"))
  expect_equal(x$identify$search_term, "crinia")
  y <- collapse(x)
  expect_s3_class(y, "query")
  expect_equal(names(y), c("type", "atlas", "url", "headers", "request"))
  z <- collect(y)
  expect_tibble(z)
  expect_equal(nrow(z), 1)
})

rm(purrr_taxa, purrr_identifiers)