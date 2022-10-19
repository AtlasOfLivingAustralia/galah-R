context("Test international atlases: Estonia")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = Estonia works", {
  expect_silent(galah_config(atlas = "Estonia"))
})

vcr::use_cassette("IA_Estonia_show_all", {
  test_that("show_all works for Estonia", {
    ## collectory
    expect_gt(nrow(show_all(collections)), 1)
    expect_gt(nrow(show_all(datasets)), 1)
    expect_gt(nrow(show_all(providers)), 1)  
    ## records
    expect_gt(nrow(show_all(assertions)), 1)
    expect_gt(nrow(show_all(fields)), 1)
    # logger
    expect_error(show_all(reasons))
    # profiles
    expect_error(show_all(profiles))
    # lists
    expect_error(show_all(lists))
  })
}) 

vcr::use_cassette("IA_Estonia_search_all", {
  test_that("search_all works for Estonia", {
    expect_equal(class(search_all(fields, "year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(search_all(taxa, "Mammalia")), 1) 
  })
})

vcr::use_cassette("IA_Estonia_show_values", {
  test_that("show_values works for Estonia", {
    expect_gt(nrow(show_field_values("basis_of_record")), 1)
    expect_error(show_list_values("a_list"))
    expect_error(show_profile_values("a_profile"))
  })
})

vcr::use_cassette("IA_Estonia_atlas_counts", {
  test_that("atlas_counts works for Estonia", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_Estonia_atlas_counts3", {
  test_that("atlas_counts works with group_by for Estonia", {
    result <- galah_call() |>
      galah_filter(year >= 2018) |>
      galah_group_by(basis_of_record) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("basis_of_record", "count"))
  })
})

test_that("atlas_occurrences returns error for Estonia", {
  expect_error(atlas_occurrences(
    filter = galah_filter(year == 2020)
  ))
})

galah_config(atlas = "Australia")