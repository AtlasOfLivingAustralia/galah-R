context("Create data request")

test_that("data request checks input", {
  skip_on_cran()
  expect_error(data_request(taxa = "vulpes"))
  expect_error(data_request(filters = "Northern Territory"))
  expect_error(data_request(occurrences = "hey"))
  expect_error(data_request(columns = "basisOfRecord"))
  expect_error(data_request(locations = "myHouse"))
})

test_that("data request returns correct data_request object", {
  skip_on_cran()
  filters <- select_filters(year = 1930)
  taxa <- select_taxa("Polytelis swainsonii")
  locations <- select_locations("POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))")
  request <- data_request(taxa = taxa, filters = filters, locations = locations)
  expect_s3_class(request, "data_request")
  expect_equal(request$taxa, taxa)
  expect_equal(request$filters, filters)
  expect_equal(request$locations, locations)
})

# test_that("data request returns correct data_request object when piping", {
#   skip("travis doesn't have latest R version")
#   taxa <- "Polytelis swainsonii"
#   req <- data_request() |> select_taxa(taxa)
#   expect_equal(req$taxa, select_taxa(taxa))
# })
