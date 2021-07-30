context("Test search_fields")

vcr::use_cassette("search_fields_all", {
  test_that("search_fields returns all field types", {
    fields <- search_fields()
    expect_setequal(
      unique(fields$type),
      c("fields", "assertions", "layers", "media", "other")
    )
    fields <- search_fields(type = "all")
    expect_setequal(
      unique(fields$type),
      c("fields", "assertions", "layers", "media", "other")
    )
  })
})

vcr::use_cassette("search_layers", {
  test_that("search_fields returns layers", {
    fields <- search_fields(type = "layer")
    expect_equal(unique(fields$type), "layers")
    
    fields <- search_fields(type = "layers")
    expect_equal(unique(fields$type), "layers")
  })
})

vcr::use_cassette("search_fields_assertions", {
  test_that("search_fields returns assertions", {
    fields <- search_fields(type = "assertions")
    expect_equal(unique(fields$type), "assertions")
  })
})

vcr::use_cassette("search_fields_text", {
  test_that("search_fields searches text correctly", {
    fields <- search_fields("precipitation")
    expect_true(all(grepl(pattern = "precipitation", fields$description,
                          ignore.case = TRUE)))
  })
})
