context("Test ALA checklist")

test_that("ALA checklist checks inputs", {
  expect_error(ala_checklist(filters = c(invalid_field = 'value')))
  expect_error(ala_checklist(filters = c(state =
                                           'Australian Capital Territory')))
})

test_that("ALA checklist returns dataframe", {
  expect_equal(class(ala_checklist(taxon_id = ala_taxa("reptilia"),
                             filters = ala_filters(list(basis_of_record =
                                              "FossilSpecimen")))),
               "data.frame")
})