context("Taxa search")

test_that("search_taxa checks inputs", {
  expect_warning(search_taxa())
})

# test_that("search_taxa check atlas", { # FIXME: is this still true?
#   galah_config(atlas = "Austria")
#   expect_error(search_taxa("Vulpes vulpes"))
#   galah_config(atlas = "Australia")
# })

test_that("search_taxa works for simple queries", {
  vcr::use_cassette("search_taxa_simple", {
    taxa <- search_taxa("Microseris lanceolata")
  })
  expect_equal(nrow(taxa), 1)
})

test_that("search_taxa works for multiple queries", {
  vcr::use_cassette("search_taxa_multiple", {
    taxa <- search_taxa(c("Eucalyptus", "Banksia", "Acacia"))
  })
  expect_equal(nrow(taxa), 3)
})


test_that("search_taxa handles data.frame input", {
  vcr::use_cassette("search_taxa_df", {
    taxa <- search_taxa(
      data.frame(genus = c("Banksia", "Microseris"), kingdom = "Plantae"))
  })
  expect_equal(nrow(taxa), 2)
})

vcr::use_cassette("search_taxa_valid_invalid", {
  test_that("search_taxa handles a mix of valid and invalid queries", {
    galah_config(verbose = TRUE)
    expect_message(taxa <- search_taxa(c("Eucalyptus", "Banksia", "Wattle", "wootle")))
    expect_equal(nrow(taxa), 4)
    galah_config(verbose = FALSE)
  })
})

vcr::use_cassette("search_taxa_invalid", {
  test_that("search_taxa gives a message for invalid names", {
    galah_config(verbose = TRUE)
    expect_message(search_taxa("bad_term"))
    galah_config(verbose = FALSE)
  })
})

test_that("search_taxa searches using multiple ranks", { # FIXME
  vcr::use_cassette("search_taxa_rank_search", {
    taxa <- search_taxa(data.frame(genus = "Acacia", kingdom = "Plantae"))
  })
    expect_s3_class(taxa, c("tbl_df", "tbl", "data.frame"))
    expect_equal(taxa$rank, "genus")
    expect_equal(nrow(taxa), 1)
})

test_that("search_identifiers searches using identifier", {
  vcr::use_cassette("search_identifiers_id_search", {
    # check different types of id
    identifier <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
               "NZOR-6-1742", "https://id.biodiversity.org.au/node/apni/2910467")
    taxa <- search_identifiers(identifier)
  })
    expect_s3_class(taxa, c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(taxa), 3)
})

vcr::use_cassette("search_identifiers_invalid", {
  test_that("search_identifiers gives a message for invalid ids", {
    galah_config(verbose = TRUE)
    expect_message(search_identifiers("1234"))
    galah_config(verbose = FALSE)
  })
})

test_that("search_taxa handles name issues", {
  expect_warning(search_taxa("Microseris"))
})

test_that("search_taxa errors nicely when piped in galah_call", {
  expect_error(galah_call() |> search_taxa("perameles"), "Can't pipe `search_taxa()")
})
