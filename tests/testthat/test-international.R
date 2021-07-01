context("Test international atlas configuration")

test_that("International Atlases work as expected", {
  skip_on_cran
  atlases <- find_atlases()$atlas
  for (atlas in atlases) {
    expect_silent(ala_config(atlas = atlas))
    expect_gt(nrow(search_fields()), 1)
    expect_gt(ala_counts(), 0)
  }
})

# reset to Aus
ala_config(atlas = "Australia")

