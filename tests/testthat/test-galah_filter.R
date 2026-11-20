test_that("`filter()` works for a single 'equals' argument", {  
  x <- galah_filter(year == 2010)
  expect_tibble(x)
  expect_equal(nrow(x), 1)
})

test_that("`filter()` gives an error for single equals sign", {
  expect_error(galah_filter(year = 2010))
})

test_that("`filter()` returns empty tibble when no arguments specified", {
  x <- galah_filter()
  expect_tibble(x)
  expect_equal(nrow(x), 0)
})
