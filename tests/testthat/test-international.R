context("Test international atlas configuration")

test_that("UK Atlas works", {
  skip_on_cran()
  ala_config(country = "UK")
  expect_gt(ala_counts(), 0)
})

test_that("Swedish Atlas works", {
  skip_on_cran()
  ala_config(country = "Sweden")
  expect_gt(ala_counts(), 0)
})

test_that("Vermont Atlas works", {
  skip("Unreliable Atlas")
  ala_config(country = "Vermont")
  expect_gt(ala_counts(), 0)
})

# reset to Aus
ala_config(country = "Australia")

