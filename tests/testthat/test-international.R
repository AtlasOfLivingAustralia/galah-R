context("Test international atlas configuration")

test_that("Other international atlas functions work", {
  skip("Slow test")
  atlases <- find_atlases()$atlas
  for (atlas in atlases) {
    galah_config(atlas = atlas)
    expect_equal(class(find_field_values("year")), "data.frame")
  }
})

test_that("Unsupported international functions fail gracefully", {
  galah_config(atlas = "Austria")
  expect_error(find_profiles(),
               "Data quality filtering is not supported for the Austria atlas.")
})

vcr::use_cassette("swedish_atlas", {
  test_that("Swedish atlas returns data", {
    expect_silent(galah_config(atlas = "Sweden"))
    expect_gt(ala_counts(), 0)
    expect_gt(nrow(search_fields()), 1)
    expect_equal(class(find_field_values("year")), "data.frame")
  })
})


# test_that("UK atlas returns data", {
#   expect_silent(galah_config(atlas = "UK"))
#   expect_gt(ala_counts(taxa = NULL), 0)
#   expect_gt(nrow(search_fields()), 1)
#   expect_equal(class(find_field_values("year")), "data.frame")
# })


vcr::use_cassette("austrian_atlas", {
  test_that("Austrian atlas returns data", {
    expect_silent(galah_config(atlas = "Austria"))
    expect_gt(ala_counts(), 0)
    expect_gt(nrow(search_fields()), 1)
    expect_equal(class(find_field_values("year")), "data.frame")
  })
})

vcr::use_cassette("guatemalan_atlas", {
  test_that("Guatemalan atlas returns data", {
    expect_silent(galah_config(atlas = "Guatemala"))
    expect_gt(ala_counts(), 0)
    expect_gt(nrow(search_fields()), 1)
    expect_equal(class(find_field_values("year")), "data.frame")
  })
})

vcr::use_cassette("spanish_atlas", {
  test_that("Spanish atlas returns data", {
    expect_silent(galah_config(atlas = "Spain"))
    expect_gt(ala_counts(), 0)
    expect_gt(nrow(search_fields()), 1)
    expect_equal(class(find_field_values("year")), "data.frame")
  })
})

# reset to Aus
galah_config(atlas = "Australia")
