context("Test international atlases: Austria")

# set verbose to off
galah_config(verbose = FALSE)

test_that("swapping to atlas = Austria works", {
  expect_silent(galah_config(atlas = "Austria"))
})

vcr::use_cassette("IA_Austria_show_all", {
  test_that("show_all works for Austria", {
    ## collectory
    expect_gt(nrow(show_all(collections)), 1)
    expect_gt(nrow(show_all(datasets)), 1)
    expect_gt(nrow(show_all(providers)), 1)   
    ## records
    expect_gt(nrow(show_all(assertions)), 1)
    expect_gt(nrow(show_all(fields)), 1)
    expect_gt(nrow(show_all(values, "year")), 1)
    # logger
    expect_gt(nrow(show_all(reasons)), 1)
    # profiles
    expect_error(show_all(profiles))
  })
}) 

vcr::use_cassette("IA_Austria_search_all", {
  test_that("search_all works for Austria", {
    expect_equal(class(search_all(fields, "year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(search_all(taxa, "Vulpes vulpes")), 1) 
  })
})

vcr::use_cassette("IA_Austria_atlas_counts", {
  test_that("atlas_counts works for Austria", {
    expect_gt(atlas_counts()$count, 0)
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_Austria_atlas_counts2", {
  test_that("atlas_counts works with group_by for Austria", {
    galah_call() |>
      galah_filter(year >= 2020)
      group_by(year) |>
      atlas_counts()
  })
})

vcr::use_cassette("IA_Austria_atlas_occurrences", {
  test_that("atlas_occurrences works for Austria", {
    skip_on_cran()
    occ <- galah_call() |>
      galah_identify("Vulpes vulpes") |>
      galah_filter(year == 1990) |>
      galah_select(year) |>
      atlas_occurrences()
      
    expect_gt(nrow(occ), 0)
    expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
  })
})

#     ## species
#     expect_gt(
#       nrow(atlas_species(identify = species, filter = filter)),
#       0
#     ) 
#     # media - not checked yet
#   })
# })

galah_config(atlas = "Australia")