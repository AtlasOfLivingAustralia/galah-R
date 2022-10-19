context("Test international atlases: Brazil")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

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
    expect_equal(nrow(search_all(taxa, "Mammalia")), 1) 
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
  test_that("atlas_counts works with galah_identify for Brazil", {
    result <- galah_call() |>
      galah_identify("Mammalia") |>
      atlas_counts()
    expect_gt(result$count, 1)
    
    result2 <- galah_call() |>
      galah_filter(class == "Mammalia") |>
      atlas_counts()
    
    expect_lt(
      sqrt((result2$count - result$count)^2) / result$count, 
      0.1) # i.e. <1% margin of error
  })
})

vcr::use_cassette("IA_Brazil_atlas_counts3", {
  test_that("atlas_counts works with group_by for Brazil", {
    result <- galah_call() |>
      galah_filter(year >= 2020) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})

test_that("atlas_occurrences works for Brazil", {
  skip_on_cran()
  galah_config(
    atlas = "Brazil",
    email = "ala4r@ala.org.au", 
    send_email = FALSE)
  occ <- galah_call() |>
    galah_identify("Mammalia") |>
    galah_filter(year == 1970) |>
    galah_select(taxon_name, year) |>
    atlas_occurrences()  
  expect_gt(nrow(occ), 0)
  expect_equal(ncol(occ), 2)
  expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
})

galah_config(atlas = "Australia")