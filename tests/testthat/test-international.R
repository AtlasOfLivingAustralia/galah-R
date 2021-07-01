context("Test international atlas configuration")

test_that("International Atlases work as expected", {
  skip_on_cran()
  atlases <- find_atlases()$atlas
  for (atlas in atlases) {
    expect_silent(ala_config(atlas = atlas))
    expect_gt(nrow(search_fields()), 1)
    expect_gt(ala_counts(), 0)
    expect_gt(ala_counts(filters = select_filters(year = 2020)), 0)
    
  }
})

test_that("Other international atlas functions work", {
  skip("Slow test")
  atlases <- find_atlases()$atlas
  for (atlas in atlases) {
    ala_config(atlas = atlas)
    expect_equal(class(find_field_values("year")), "data.frame")
  }
})

# reset to Aus
ala_config(atlas = "Australia")

