context("Test international atlases: Portugal")

# set verbose to off
galah_config(verbose = FALSE, run_checks = FALSE)

test_that("swapping to atlas = United Kingdom works", {
  expect_silent(galah_config(atlas = "United Kingdom"))
})

# putting this first to ensure show_all_fields gets cached properly
# if this doesn't happen, later field name checks call the wrong atlas (Aus)
# leading later tests to fail
vcr::use_cassette("IA_United_Kingdom_show_all_fields", {
  test_that("show_all works for United Kingdom", {
    result <- show_all_fields()
    expect_gt(nrow(result), 1)
  })
})
  
vcr::use_cassette("IA_United_Kingdom_show_all", {
  test_that("show_all works for United Kingdom", {
    ## collectory
    expect_gt(nrow(show_all(collections)), 1)
    expect_gt(nrow(show_all(datasets)), 1)
    expect_gt(nrow(show_all(providers)), 1)  
    ## records
    expect_gt(nrow(show_all(assertions)), 1)
    # logger
    expect_gt(nrow(show_all(reasons)), 1)
    # profiles
    expect_error(show_all(profiles))
    # lists
    expect_gt(nrow(show_all(lists)), 1)
  })
}) 

vcr::use_cassette("IA_search_taxa_multiple_UK", {
  test_that("search_taxa works for multiple queries", {
    taxa <- search_taxa(c("Vulpes vulpes", "Meles meles"))
    expect_equal(nrow(taxa), 2)
  })
})

vcr::use_cassette("IA_search_taxa_types_UK", {
  test_that("search_taxa doesn't break with typos", {
    expect_silent(search_taxa("Vlpes"))
  })
})

vcr::use_cassette("IA_United_Kingdom_search_all", {
  test_that("search_all works for United Kingdom", {
    expect_equal(class(search_all(fields, "year")), 
                 c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(search_all(taxa, "Mammalia")), 1) 
  })
})

vcr::use_cassette("IA_United_Kingdom_show_field_values", {
  test_that("show_field_values works for United Kingdom", {
    result <- show_field_values("basis_of_record")
    expect_gt(nrow(result), 1)
  })
})

vcr::use_cassette("IA_United_Kingdom_show_values", {
  test_that("show_values works for United Kingdom", {
    expect_gt(nrow(show_list_values("dr556")), 1)
    expect_error(show_profile_values("a_profile"))
  })
})

vcr::use_cassette("IA_United_Kingdom_atlas_counts_records", {
  test_that("atlas_counts w records  works for United Kingdom", {
    expect_gt(atlas_counts()$count, 0)
  })
})

vcr::use_cassette("IA_United_Kingdom_atlas_counts_species", {
  test_that("atlas_counts w species works for United Kingdom", {
    expect_gt(atlas_counts(type = "species")$count, 0)
  })
})

vcr::use_cassette("IA_United_Kingdom_atlas_counts2", {
  test_that("atlas_counts works with galah_identify for United Kingdom", {
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

vcr::use_cassette("IA_United_Kingdom_atlas_counts3", {
  test_that("atlas_counts works with group_by for United Kingdom", {
    result <- galah_call() |>
      galah_filter(year >= 2020) |>
      galah_group_by(year) |>
      atlas_counts()
    expect_gt(nrow(result), 1)
    expect_equal(names(result), c("year", "count"))
  })
})

# FIXME: this is currently failing to download records
# test_that("atlas_occurrences works for United Kingdom", {
#   skip_on_cran()
#   galah_config(
#     atlas = "United Kingdom",
#     email = "ala4r@ala.org.au",
#     send_email = FALSE)
#   occ <- galah_call() |>
#     galah_identify("Mammalia") |>
#     galah_filter(year <= 1900 & basis_of_record == PreservedSpecimen) |>
#     galah_select(taxon_name, year) |>
#     atlas_occurrences()
#   expect_gt(nrow(occ), 0)
#   expect_equal(ncol(occ), 2)
#   expect_s3_class(occ, c("tbl_df", "tbl", "data.frame"))
# })

galah_config(atlas = "Australia")