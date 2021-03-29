context('Taxa search')


test_that("select_taxa checks inputs", {
  skip_on_cran()
  expect_error(select_taxa())
  expect_error(select_taxa(term_type = 'bad_term'))
  expect_error(select_taxa("Varanus varius", children = 'false'))
})

test_that("child_concepts behaves correctly", {
  # species with no children should return a messag
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
  expect_equal(
    nrow(select_taxa(term = "https://id.biodiversity.org.au/node/apni/2902929",
                  term_type = "identifier")), 1)
  expect_message(
    select_taxa(term = "https://id.biodiversity.org.au/node/apni/2902929",
             term_type = "name"))
})

test_that("select_taxa handles name searches", {
  skip_on_cran()
  expect_equal(nrow(select_taxa(term = "Microseris lanceolata",
                             term_type = "name")), 1)
  expect_message(select_taxa(term = "Microseris lanceolata",
                          term_type = "identifier"))
  # Handle multiple names
  expect_equal(nrow(select_taxa(term = c("Eucalyptus", "Banksia", "Acacia"))), 3)
  
  # Handle list of multiple names
  expect_equal(nrow(select_taxa(term = list("Eucalyptus", "Banksia", "Acacia"))),
               3)
  
  # Handle mix of valid and invalid names
  expect_message(
    expect_equal(nrow(select_taxa(term = c("Eucalyptus", "Banksia", "Wattle"))),
                 3))
  
  # Handle a dataframe input
  taxa_df <- data.frame(genus = c("Banksia", "Microseris"),
                        kingdom = "Plantae")
  expect_equal(nrow(select_taxa(term = taxa_df)), 2)
  
  taxa_df <- data.frame(genus = c("Banksia", "Microseris"))
  expect_message(expect_equal(nrow(select_taxa(term = taxa_df)), 2))
})

test_that("ala taxa returns counts for species", {
  skip_on_cran()
  expect_true("count" %in% colnames(select_taxa(term = "Thylacinus cynocephalus",
                                        counts = TRUE)))
})

test_that("select_taxa handles name issues", {
  expect_warning(select_taxa("Microseris"))
})

test_that("select_taxa returns children for multiple names", {
  expect_equal(
    nrow(select_taxa(c("Osphranter", "Dasyurus"), children = TRUE)),
    sum(nrow(select_taxa("Osphranter", children = TRUE)),
        nrow(select_taxa("Dasyurus", children = TRUE))))
})

