quiet_config <- purrr::quietly(galah_config)
x <- quiet_config(atlas = "GBIF")

test_that("galah_filter() returns predicates for GBIF", {
  x <- galah_filter(year == 2024) 
  inherits(x, "list") |>
    expect_true()
  expect_equal(names(x[[1]]),
               c("type", "key", "value"))
  expect_equal(x[[1]],
               list(type = "equals", 
                    key = "YEAR",
                    value = "2024"))
})

# FIXME: `check_fields()` not tested for GBIF - try sending invalid fields to `filter()`

# only the above test contains information rn
# 
# test_that("filter() handles multiple queries for GBIF", {
#   result <- galah_call() |>
#     filter(year == 2024, basisOfRecord == "HUMAN_OBSERVATION") |>
#     collapse()
#   
#   str(result)
# })
# 
# test_that("filter() handles multiple queries including != for GBIF", {
#   result <- galah_call() |>
#     filter(year == 2024, countryCode != "AU")
#   
#   str(result)
# })
# 
# test_that("filter() handles AND for GBIF", {
#   result <- galah_call() |>
#     filter(year == 2024 & countryCode != "AU")
#   
#   str(result)
# })
# 
# # assertions?
# 
# test_that("filter() handles `between()` for GBIF", {
#   galah_call() |>
#     filter(dplyr::between(year, 2010, 2020)) |>
#     str()
# })
# 
# 
# test_that("filter() handles %in% for GBIF", {
#   galah_call() |>
#     filter(year %in% c(2010, 2020)) |>
#     str()
# })
# 
# test_that("filter() handles !() for GBIF", {
#   galah_call() |>
#     filter(!(year %in% c(2010, 2020))) |>
#     str()
# })
# 
# test_that("filter() handles is.na() for GBIF", {
#   galah_call() |>
#     filter(is.na(country)) |>
#     str()
# })
# 
# test_that("filter() handles !is.na() for GBIF", {
#   galah_call() |>
#     filter(!is.na(country)) |>
#     str()
# })
# 
# test_that("filter() handles c() for GBIF", {
#   # check when supplied directly
#   galah_call() |>
#     filter(country == c("AU", "UK", "AZ")) |>
#     str()
#   
#   # and as a vector
#   country_vector <- c("AU", "UK", "AZ")
#   galah_call() |>
#     filter(country == country_vector) |>
#     str()
#   # effectively parses this as 'in' as per GBIF instructions
# })
# 
# # missing `within` (galah_geolocate())
# 
# # missing `geoDistance` (galah_radius())

x <- quiet_config(atlas = "ALA")
rm(x, quiet_config)