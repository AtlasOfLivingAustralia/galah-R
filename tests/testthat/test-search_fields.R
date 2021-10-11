context("Test search_fields")


test_that("search_fields returns all field types", {
  vcr::use_cassette("search_fields_all", {
    fields <- search_fields()
    fields_all <- search_fields(type = "all")
  })
  expect_setequal(
    unique(fields$type),
    c("fields", "assertions", "layers", "media", "other")
  )
  expect_setequal(
    unique(fields_all$type),
    c("fields", "assertions", "layers", "media", "other")
  )
})


test_that("search_fields returns layers", {
  vcr::use_cassette("search_layers", {
    field_layer <- search_fields(type = "layer")
    field_layers <- search_fields(type = "layers")
  })
    expect_equal(unique(field_layer$type), "layers")
    expect_equal(unique(field_layers$type), "layers")
})


test_that("search_fields returns assertions", {
  vcr::use_cassette("search_fields_assertions", {
    fields <- search_fields(type = "assertions")
  })
  expect_equal(unique(fields$type), "assertions")
})


test_that("search_fields searches text correctly", {
  vcr::use_cassette("search_fields_text", {
    fields <- search_fields("precipitation")
  })
    expect_true(all(grepl(pattern = "precipitation", fields$description,
                          ignore.case = TRUE)))
})
