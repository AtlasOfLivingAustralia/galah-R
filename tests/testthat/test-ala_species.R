context("Test ala_species")

test_that("ala_species checks inputs", {
  expect_error(ala_species(filters = select_filters(invalid_field = 'value')))
  expect_error(ala_species(filters = c(state =
                                           'Australian Capital Territory')))
})

test_that("ala_species returns dataframe", {
  expect_equal(class(ala_species(taxa = select_taxa("reptilia"),
                                   filters = select_filters(basisOfRecord =
                                                                "PreservedSpecimen"))),
               "data.frame")
})

test_that("ala_species returns a sensible result", {
  expect_equal(nrow(ala_species(taxa = select_taxa("Osphranter"))), 4)
})

test_that("ala_species caches results as expected", {
  ala_config(caching = TRUE)
  filters <- select_filters(decade = seq(1800, 1850),
                            genus = "Acacia")
  species <- ala_species(filters = filters)
  expect_message(species2 <- ala_species(filters = filters))
  expect_equal(species, species2)
  ala_config(caching = FALSE)
})