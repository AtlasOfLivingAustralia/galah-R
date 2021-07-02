context('Taxa search')

test_that("select_taxa checks inputs", {
  skip_on_cran()
  expect_error(select_taxa())
  # unrecognised name
  expect_message(select_taxa('bad_term'))
  # unrecognised id
  expect_message(select_taxa("1234"))
  expect_error(select_taxa("Varanus varius", children = 'false'))
})

test_that("select_taxa check atlas", {
  skip_on_cran()
  ala_config(atlas = "Austria")
  expect_error(select_taxa("Vulpes vulpes"))
  ala_config(atlas = "Australia")
})

test_that("child_concepts behaves correctly", {
  # species with no children should return a message
  skip_on_cran()
  expect_message(
    child_concepts("urn:lsid:biodiversity.org.au:afd.taxon:ac61fd14-4950-4566-b384-304bd99ca75f"))
  
  expect_is(child_concepts("urn:lsid:biodiversity.org.au:afd.taxon:f05d7036-e74b-4468-858d-1f7d78470298"), "data.frame")
  # correct number of children are returned
})

test_that("child concepts returns expected number of children", {
  # skip while this is failing on travis
  skip_on_cran()
  expect_equal(nrow(select_taxa("Hydromys", children = TRUE)), 2)
})


test_that("select_taxa searches at provided rank", {
  skip_on_cran()
  expect_equal(select_taxa(list(genus = "Acacia"))$rank, "genus")
  
  expect_equal(select_taxa(list(specificEpithet = "dioica",
                                genus = "Wurmbea"))$rank, "species")
  
  # expect validation error for bad rank
  expect_error(select_taxa(list(kingdom = "Animalia", bad_rank = "species")))
})

test_that("select_taxa handles identifier searches", {
  skip_on_cran()
  query <- c("urn:lsid:biodiversity.org.au:afd.taxon:08b9a1f0-62ae-45ca-9208-e773b00021ed",
           "NZOR-6-1742", "https://id.biodiversity.org.au/node/apni/2910467")
  expect_equal(
    nrow(select_taxa(query)), 3)
})

test_that("select_taxa handles name searches", {
  skip_on_cran()

  expect_equal(nrow(select_taxa("Microseris lanceolata")), 1)
  select_taxa("Microseris lanceolata")
  # Handle multiple names
  expect_equal(nrow(select_taxa(c("Eucalyptus", "Banksia", "Acacia"))), 3)
  
  # Handle list of multiple names
  expect_equal(nrow(select_taxa(list("Eucalyptus", "Banksia", "Acacia"))),
               3)
  
  # Handle mix of valid and invalid names
  expect_message(
    expect_equal(nrow(select_taxa(c("Eucalyptus", "Banksia", "Wattle"))),
                 3))
  
  # Handle a dataframe input
  taxa_df <- data.frame(genus = c("Banksia", "Microseris"),
                        kingdom = "Plantae")
  expect_equal(nrow(select_taxa(taxa_df)), 2)
  
  taxa_df <- data.frame(genus = c("Banksia", "Microseris"))
  expect_message(expect_equal(nrow(select_taxa(taxa_df)), 2))
})

test_that("select taxa returns counts for species", {
  skip_on_cran()
  expect_true("count" %in% colnames(select_taxa("Thylacinus cynocephalus",
                                        counts = TRUE)))
})

test_that("select_taxa handles name issues", {
  expect_warning(select_taxa("Microseris"))
})

test_that("select_taxa returns children for multiple names", {
  skip_on_cran()
  expect_equal(
    nrow(select_taxa(c("Osphranter", "Dasyurus"), children = TRUE)),
    sum(nrow(select_taxa("Osphranter", children = TRUE)),
        nrow(select_taxa("Dasyurus", children = TRUE))))
})

test_that("select_taxa errors if number of cols in data doesn't match", {
  skip_on_cran()
  expect_equal(unique(select_taxa(c("Animalia", "Fungi"), children = TRUE)$kingdom),
               c("Animalia", "Fungi"))
})

test_that("select_taxa returns extended taxonomy", {
  skip_on_cran()
  expect_true("subfamily" %in% names(select_taxa("Anas", all_ranks = TRUE)))
})
