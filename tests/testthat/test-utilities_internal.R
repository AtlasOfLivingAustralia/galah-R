context("Test helper functions")

test_that("Multiple negated queries are joined with 'AND'", {
  expect_match(build_taxa_query(c("id1", "id2"), FALSE), "AND")
  expect_match(query_term(name = "test", value = c("a", "b"), include = FALSE),
               "AND")
})