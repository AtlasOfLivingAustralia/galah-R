context("Test international atlases: Brazil")

# set verbose to off
galah_config(verbose = FALSE)

test_that("swapping to atlas = Brazil works", {
  expect_silent(galah_config(atlas = "Brazil"))
})

vcr::use_cassette("IA_Brazil_show_all", {
  test_that("show_all works for Brazil", {
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
    expect_gt(nrow(show_all(lists)), 1)
  })
}) 

vcr::use_cassette("IA_Brazil_search_all", {
  test_that("search_all works for Brazil", {
    expect_equal(class(search_all(fields, "year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(search_all(taxa, "Vulpes vulpes")), 1) 
  })
})

vcr::use_cassette("IA_Brazil_show_values", {
  test_that("show_values works for Brazil", {
    expect_gt(nrow(show_field_values("basis_of_record")), 1)
    expect_gt(nrow(show_list_values("drt1565630923841")), 1)
    expect_error(show_profile_values("profile"))
  })
})

vcr::use_cassette("IA_Brazil_atlas_counts", {
  test_that("atlas_counts works for Brazil", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_Brazil_atlas_counts2", {
  test_that("atlas_counts works with group_by for Brazil", {
    result <- galah_call() |>
      galah_identify("Vulpes vulpes") |>
      galah_filter(year >= 2020) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})
