context("Test ala_species")

test_that("ala_species checks inputs", {
  expect_error(ala_species(filters = select_filters(invalid_field = 'value')))
  expect_error(ala_species(filters = c(state =
                                           'Australian Capital Territory')))
})

test_that("ala_species returns dataframe", {
  expect_equal(class(ala_species(taxa = select_taxa("reptilia"),
                                   filters = select_filters(basis_of_record =
                                                                "FossilSpecimen"))),
               "data.frame")
})

test_that("ala_species returns a sensible result", {
  expect_equal(nrow(ala_species(taxa = select_taxa("Osphranter"))), 4)
})