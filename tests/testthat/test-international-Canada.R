context("Test international atlases: Canada")

# set verbose to off
galah_config(verbose = FALSE)

test_that("swapping to atlas = Canada works", {
  expect_silent(galah_config(atlas = "Canada"))
})

vcr::use_cassette("IA_Canada_show_all", {
  test_that("show_all works for Canada", {
    ## collectory
    expect_gt(nrow(show_all(collections)), 1)
    expect_gt(nrow(show_all(datasets)), 1)
    expect_gt(nrow(show_all(providers)), 1)  
    ## records
    expect_gt(nrow(show_all(assertions)), 1)
    expect_gt(nrow(show_all(fields)), 1)
    # logger
    expect_gt(nrow(show_all(reasons)), 1)
    # profiles
    expect_error(show_all(profiles))
    # lists
    expect_error(show_all(lists))
  })
}) 