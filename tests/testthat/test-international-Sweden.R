context("Test international atlases: Sweden")

# set verbose to off
galah_config(verbose = FALSE)

test_that("swapping to atlas = Sweden works", {
  expect_silent(galah_config(atlas = "Sweden"))
})

vcr::use_cassette("IA_Sweden_show_all", {
  test_that("show_all works for Sweden", {
    ## collectory
    # expect_gt(nrow(show_all(collections)), 1) # collections API exists,
      # but does not contain data
    expect_gt(nrow(show_all(datasets)), 1)
    expect_gt(nrow(show_all(providers)), 0)  
    ## records
    expect_gt(nrow(show_all(assertions)), 1)
    # expect_gt(nrow(show_all(fields)), 1) # errors at result$type
    # logger
    expect_gt(nrow(show_all(reasons)), 1)
    # profiles
    expect_error(show_all(profiles))
    # lists
    expect_error(show_all(lists))
  })
}) 