context("Test international atlas configuration")

# set verbose to off
galah_config(verbose = FALSE)

test_that("Other international atlas functions work", {
  skip("Slow test")
  atlases <- find_atlases()$atlas
  for (atlas in atlases) {
    galah_config(atlas = atlas)
    expect_equal(class(search_field_values("year")), 
                 c("tbl_df", "tbl", "data.frame"))
  }
})

vcr::use_cassette("international_atlases_Sweden", {
  test_that("Swedish atlas returns data", {
    expect_silent(galah_config(atlas = "Sweden"))
    expect_gt(atlas_counts()$count, 0)
    expect_gt(nrow(show_all_fields()), 1)
    expect_equal(class(search_field_values("year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_error(show_all_profiles())
    expect_error(show_all_reasons())
  })
})

vcr::use_cassette("international_atlases_UK", {
  test_that("UK atlas returns data", {
    expect_silent(galah_config(atlas = "UK"))
    expect_gt(atlas_counts()$count, 0)
    expect_gt(nrow(show_all_fields()), 1)
    expect_equal(
      class(search_field_values("year")), 
      c("tbl_df", "tbl", "data.frame"))
    expect_error(show_all_profiles())
    expect_error(show_all_reasons())
  })
})

vcr::use_cassette("international_atlases_Austria", {
  test_that("Austrian atlas returns data", {
    expect_silent(galah_config(atlas = "Austria"))
    expect_gt(atlas_counts()$count, 0)
    expect_gt(nrow(show_all_fields()), 1)
    expect_equal(class(search_field_values("year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_error(show_all_profiles())
    expect_error(show_all_reasons())
  })
})

vcr::use_cassette("international_atlases_Guatemala", {
  test_that("Guatemalan atlas returns data", {
    expect_silent(galah_config(atlas = "Guatemala"))
    expect_gt(atlas_counts()$count, 0)
    expect_gt(nrow(show_all_fields()), 1)
    expect_equal(class(search_field_values("year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_error(show_all_profiles())
    expect_error(show_all_reasons())
  })
})

vcr::use_cassette("international_atlases_Spain", {
  test_that("Spanish atlas returns data", {
    expect_silent(galah_config(atlas = "Spain"))
    expect_gt(atlas_counts()$count, 0)
    expect_gt(nrow(show_all_fields()), 1)
    expect_equal(class(show_all_fields()), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_gt(nrow(show_all_reasons()), 1)
    expect_error(show_all_profiles())
  })
})

# reset to Aus
galah_config(atlas = "Australia")
