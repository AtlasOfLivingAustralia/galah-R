context("Test helper functions")

test_that("Negated queries have a minus sign", {
  expect_match(query_term(name = "test", value = c("a", "b"), include = FALSE),
               "-")
})

test_that("Taxa arguments are checked", {
  galah_config(verbose = FALSE)
  expect_error(check_taxa_arg("Vulpes vulpes"))
  expect_silent(check_taxa_arg("1234"))
  
})

vcr::use_cassette("select_taxa", {
  expect_silent(check_taxa_arg(select_taxa("Vulpes vulpes")))
})