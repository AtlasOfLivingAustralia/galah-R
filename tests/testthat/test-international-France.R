context("Test international atlases: France")

# set verbose to off
galah_config(verbose = FALSE)

test_that("swapping to atlas = France works", {
  expect_silent(galah_config(atlas = "France"))
})

vcr::use_cassette("IA_France_show_all", {
  test_that("show_all works for France", {
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