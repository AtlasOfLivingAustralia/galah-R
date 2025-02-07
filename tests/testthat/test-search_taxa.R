test_that("search_taxa checks inputs", {
  expect_error(search_taxa())
})

test_that("search_taxa works for simple queries", {
  skip_if_offline(); skip_on_ci()
  taxa <- search_taxa("Microseris lanceolata")
  expect_equal(nrow(taxa), 1)
})

test_that("search_taxa works for multiple queries", {
  skip_if_offline(); skip_on_ci()
  taxa <- search_taxa(c("Eucalyptus", "Banksia", "Acacia"))
  expect_equal(nrow(taxa), 3)
})

test_that("search_taxa handles data.frame input", {
  skip_if_offline(); skip_on_ci()
  taxa <- search_taxa(
    data.frame(genus = c("Banksia", "Microseris"), kingdom = "Plantae"))
  expect_equal(nrow(taxa), 2)
})

test_that("search_taxa handles a mix of valid and invalid queries", {
  skip_if_offline(); skip_on_ci()
  galah_config(verbose = TRUE)
  expect_message(taxa <- search_taxa(c("Eucalyptus", "Banksia", "Wattle", "wootle")))
  expect_equal(nrow(taxa), 4)
  galah_config(verbose = FALSE)
})

test_that("search_taxa gives a message for invalid names", {
  skip_if_offline(); skip_on_ci()
  galah_config(verbose = TRUE)
  expect_message(search_taxa("bad_term"))
  galah_config(verbose = FALSE)
})

test_that("search_taxa searches using multiple ranks", {
  skip_if_offline(); skip_on_ci()
  taxa <- search_taxa(data.frame(genus = "Acacia", kingdom = "Plantae"))
  expect_s3_class(taxa, c("tbl_df", "tbl", "data.frame"))
  expect_equal(taxa$rank, "genus")
  expect_equal(nrow(taxa), 1)
})

test_that("`search_identifiers()` works via `search_all()`", {
  skip_if_offline(); skip_on_ci()
  id <- "urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed"
  search <- search_identifiers(id)
  search2 <- search_all(identifiers, id)

  expect_equal(attributes(search)$call, "identifiers")
  expect_s3_class(search, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(search), 1)
  expect_equal(search, search2)
})

test_that("search_identifiers searches using identifier", {
  skip_if_offline(); skip_on_ci()
  # check different types of id
  identifier <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
                  "NZOR-6-1742", "https://id.biodiversity.org.au/node/apni/2910467")
  taxa <- search_identifiers(identifier)
  expect_s3_class(taxa, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(taxa), 3)
})

test_that("search_identifiers works when one value is missing", {
  skip_if_offline(); skip_on_ci()
  # check different types of id
  identifier <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
                  "something")
  galah_config(verbose = TRUE)
  expect_message({taxa <- search_identifiers(identifier)})
  expect_s3_class(taxa, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(taxa), 2)
  galah_config(verbose = FALSE)
})

test_that("search_identifiers gives a message for invalid ids", {
  skip_if_offline(); skip_on_ci()
  galah_config(verbose = TRUE)
  expect_message(search_identifiers("1234"))
  galah_config(verbose = FALSE)
})

test_that("search_taxa gives an error when homonyms are returned", {
  skip_if_offline(); skip_on_ci()
  expect_warning(search_taxa("ACANTHOCEPHALA"))
})

test_that("search_taxa handles name issues", {
  skip_if_offline(); skip_on_ci()
  expect_warning(search_taxa("Microseris"))
})

test_that("search_taxa handles multiple issues", {
  skip_if_offline(); skip_on_ci()
  expected <- c("homonym, parentChildSynonym")
  result <- search_taxa("Gallinago sp.")
  
  expect_no_error(search_taxa("Gallinago sp."))
  expect_equal(length(result$issues), 1)
  expect_equal(result$issues, expected) # NOTE: This test might be too rigid
  
})

test_that("search_taxa errors nicely when piped in galah_call", {
  expect_error(galah_call() |> search_taxa("perameles"), "Can't pipe `search_taxa()")
})

test_that("`request_metadata()` works for `type = 'taxa'`", {
  skip_if_offline(); skip_on_ci()
  x <- request_metadata() |>
    identify("crinia") 
  expect_s3_class(x, "metadata_request")
  expect_equal(names(x), c("type", "identify"))
  expect_equal(x$identify$search_term, "crinia")
  y <- collapse(x)
  expect_s3_class(y, "query")
  expect_equal(names(y), c("type", "url", "headers"))
  z <- collect(y)
  expect_s3_class(z, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(z), 1)
})
