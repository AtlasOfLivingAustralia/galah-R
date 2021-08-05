context("Taxa search")

test_that("select_taxa checks inputs", {
  expect_error(select_taxa())
  expect_warning(
    expect_error(select_taxa("Varanus varius", children = "false")))
})

test_that("select_taxa check atlas", {
  galah_config(atlas = "Austria")
  expect_error(select_taxa("Vulpes vulpes"))
  galah_config(atlas = "Australia")
})

vcr::use_cassette("select_taxa_simple", {
  test_that("select_taxa works for simple queries", {
    taxa <- select_taxa("Microseris lanceolata")
    expect_equal(nrow(taxa), 1)
  })
})

vcr::use_cassette("select_taxa_multiple", {
  test_that("select_taxa works for multiple queries", {
    taxa <- select_taxa(c("Eucalyptus", "Banksia", "Acacia"))
    expect_equal(nrow(taxa), 3)
  })
})

vcr::use_cassette("select_taxa_df", {
  test_that("select_taxa handles data.frame input", {
    taxa <- select_taxa(
      data.frame(genus = c("Banksia", "Microseris"), kingdom = "Plantae"))
    expect_equal(nrow(taxa), 2)
  })
})

vcr::use_cassette("select_taxa_valid_invalid", {
  test_that("select_taxa handles a mix of valid and invalid queries", {
    expect_message(taxa <- select_taxa(c("Eucalyptus", "Banksia", "Wattle")))
    expect_equal(nrow(taxa), 3)
  })
})

vcr::use_cassette("select_taxa_invalid", {
  test_that("select_taxa gives a message for invalid ids", {
    # unrecognised name
    expect_message(select_taxa("bad_term"))
    # unrecognised id
    expect_message(select_taxa("1234", is_id = TRUE))
  })
})

vcr::use_cassette("select_taxa_extended", {
  test_that("select_taxa uses additional arguments", {
    expect_warning(
      taxa <- select_taxa("Anas", all_ranks = TRUE, children = TRUE,
                          counts = TRUE)
      )
    expect_true("subfamily" %in% names(taxa))
    expect_gt(nrow(taxa), 1)
    expect_true("count" %in% names(taxa))
  })
})

vcr::use_cassette("child_concepts", {
  test_that("child_concepts behaves correctly", {
    id <- "urn:lsid:biodiversity.org.au:afd.taxon:e4c87583-08ed-4183-8653-c8f487a93735"
    children <- child_concepts(id)
    expect_s3_class(children, "data.frame")
    expect_equal(nrow(children), 1)
  })
}, preserve_exact_body_bytes = TRUE)

vcr::use_cassette("select_taxa_rank_search", {
  test_that("select_taxa searches using multiple ranks", {
    taxa <- select_taxa(list(genus = "Acacia", kingdom = "Plantae"))
    expect_s3_class(taxa, "data.frame")
    expect_equal(taxa$rank, "genus")
    expect_equal(nrow(taxa), 1)
  })
})

vcr::use_cassette("select_taxa_id_search", {
  test_that("select_taxa searches using identifier", {
    # check different types of id
    query <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
               "NZOR-6-1742", "https://id.biodiversity.org.au/node/apni/2910467")
    taxa <- select_taxa(query)
    expect_s3_class(taxa, "data.frame")
    expect_equal(nrow(taxa), 3)
  })
})

test_that("select_taxa handles name issues", {
  expect_warning(select_taxa("Microseris"))
})
