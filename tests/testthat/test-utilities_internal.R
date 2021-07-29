context("Test helper functions")

test_that("Multiple negated queries are joined with 'AND'", {
  expect_match(build_taxa_query(c("id1", "id2"), FALSE), "AND")
  expect_match(query_term(name = "test", value = c("a", "b"), include = FALSE),
               "AND")
})

test_that("Taxa arguments are checked", {
  ala_config(verbose = FALSE)
  expect_error(check_taxa_arg("Vulpes vulpes"))
  expect_silent(check_taxa_arg("1234"))
  
})

vcr::use_cassette("select_taxa", {
  expect_silent(check_taxa_arg(select_taxa("Vulpes vulpes")))
})