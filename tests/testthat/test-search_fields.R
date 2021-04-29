context("Test search_fields")

test_that("search_fields returns correct types of field", {
  expect_setequal(unique(search_fields(type = "all")$type),
               c("fields", "assertions", "layers", "media"))
  expect_equal(unique(search_fields(type = "assertions")$type),
               "assertions")
  expect_error(search_fields(type = "layer"))
})

test_that("search_fields searches text correctly", {
  expect_true(all(grepl(pattern = "precipitation",
                        search_fields("precipitation")$description,
                        ignore.case = TRUE)))
})