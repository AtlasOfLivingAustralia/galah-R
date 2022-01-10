context("Taxa search")

test_that("search_taxa checks inputs", {
  expect_error(search_taxa())
  expect_error(search_taxa("Varanus varius", children = "false")) 
})

test_that("search_taxa check atlas", {
  galah_config(atlas = "Austria")
  expect_error(search_taxa("Vulpes vulpes"))
  galah_config(atlas = "Australia")
})

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
    expect_message(taxa <- search_taxa(c("Eucalyptus", "Banksia", "Wattle")))
    expect_equal(nrow(taxa), 3)
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

test_that("find_taxa searches using identifier", {
  vcr::use_cassette("find_taxa_id_search", {
    # check different types of id
    identifier <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
               "NZOR-6-1742", "https://id.biodiversity.org.au/node/apni/2910467")
    taxa <- find_taxa(identifier)
  })
    expect_s3_class(taxa, "data.frame")
    expect_equal(nrow(taxa), 3)
})

vcr::use_cassette("find_taxa_invalid", {
  test_that("find_taxa gives a message for invalid ids", {
    galah_config(verbose = TRUE)
    expect_warning(find_taxa("1234"))
    galah_config(verbose = FALSE)
  })
})

test_that("search_taxa handles name issues", {
  expect_warning(search_taxa("Microseris"))
})
