test_that("atlas_occurrences fails nicely if no email is provided", {
  galah_config(email = "", run_checks = FALSE)
  expect_error({
    galah_call() |>
      filter(year == 1900) |>
      compute()
  })
  galah_config(email = "ala4r@ala.org.au", run_checks = TRUE)
})

test_that("atlas_occurrences doesn't allow large downloads", {
  galah_config(atlas = "Australia")
  expect_error(atlas_occurrences())
})

test_that("atlas_occurrences gives a nice error for invalid emails", {
  galah_config(email = "test@test.org.au")
  expect_error({
    galah_call() |>
      identify("Perameles") |>
      compute()
  })
  galah_config(email = "ala4r@ala.org.au")
})

test_that("collapse(type = 'occurrences') creates an object", {
  skip_if_offline(); skip_on_ci()
  result <- galah_call() |> 
    identify("Perameles") |>
    collapse()
  # expect_equal(length(result), 4)
  expect_true(inherits(result, "query"))
  expect_equal(result$type, "data/occurrences")
})

test_that("`compute(type = 'occurrences')` works", {
  skip_if_offline(); skip_on_ci()
  base_query <- galah_call() |>
    identify("Vulpes vulpes") |>
    filter(year <= 1900, 
           basisOfRecord == "PRESERVED_SPECIMEN")
  # collapse
  query_collapse <- collapse(base_query)
  expect_true(inherits(query_collapse, "query"))
  expect_equal(length(query_collapse), 4)
  expect_equal(query_collapse$type, "data/occurrences")
  # compute
  response <- compute(base_query)
  expect_true(inherits(response, "computed_query"))
  expect_true(response$type == "data/occurrences")
  expect_equal(names(response),
               c("type",
                 "status",
                 "total_records",
                 "queue_size",
                 "status_url",
                 "cancel_url",
                 "search_url",
                 "fields"))
})

# test all filters and type of columns in one call
test_that("atlas_occurrences accepts all narrowing functions inline", { 
  skip_if_offline(); skip_on_ci()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "occurrenceStatus", "stateProvince", 
                     "ZERO_COORDINATE")
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  base_query <- galah_call(type = "occurrences") |>
    identify("Polytelis swainsonii") |>
    filter(year == 2018) |>
    select(group = "basic", stateProvince, ZERO_COORDINATE) |>
    st_crop(poly)
  # w <- collapse(base_query)
  # collect with wait = FALSE - ensure `type` is specified
  x <- compute(base_query)
  expect_equal(names(x),
               c("type",
                 "status",
                 "total_records",
                 "queue_size",
                 "status_url",
                 "cancel_url",
                 "search_url",
                 "fields"))
  expect_s3_class(x, "computed_query")
  # collect with wait = TRUE
  y <- collect(x, wait = TRUE)    
  expect_s3_class(y, c("tbl_df", "tbl", "data.frame"))
  expect_setequal(names(y), expected_cols)
  expect_equal(unique(y$stateProvince), "New South Wales")
})

# repeat above using `galah_` functions
test_that("atlas_occurrences accepts all narrowing functions in pipe", { 
  skip_if_offline(); skip_on_ci()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "dataResourceName", "occurrenceStatus", "stateProvince", 
                     "ZERO_COORDINATE", "COORDINATE_INVALID")
  poly <- "POLYGON((146.7 -34.6,147.9 -34.6,147.9 -35.7,146.7 -35.7,146.7 -34.6))"
  occ <- galah_call() |>
    galah_filter(year >= 2018) |>
    galah_select(group = "basic", 
                 stateProvince, 
                 ZERO_COORDINATE, 
                 COORDINATE_INVALID) |>
    galah_identify("Polytelis swainsonii") |> 
    galah_geolocate(poly) |>
    atlas_occurrences()
  expect_setequal(names(occ), expected_cols)
  expect_equal(unique(occ$stateProvince), "New South Wales")
})

test_that("atlas_occurrences() and friends accept a file name", {
  skip_if_offline(); skip_on_ci()
  # set up directory for testing purposes
  directory <- "TEMP"
  unlink(directory, recursive = TRUE)
  dir.create(directory)
  galah_config(directory = directory)
  # set up query
  base_query <- galah_call() |>
    galah_filter(year <= 1970) |>
    galah_select(group = "basic") |>
    galah_identify("Crinia tinnula")
  # base_query |> count() |> collect() # n = 49 on 2023-11-15
  # test `atlas_occurrences`
  occ1 <- base_query |> 
    atlas_occurrences(file = "crinia_file")
  expect_s3_class(occ1, c("tbl_df", "tbl", "data.frame"))
  expect_true(any(list.files(directory) == "crinia_file.zip"))
  # test `collect`
  occ2 <- base_query |> collect(file = "crinia_collect")
  expect_equal(occ1, occ2)
  expect_true(any(list.files(directory) == "crinia_collect.zip"))
  # test DOIs
  doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
  occ3 <- atlas_occurrences(doi = doi, file = "test_doi")
  expect_true(any(list.files(directory) == "test_doi.zip"))
  # doi with collect
  occ3 <- atlas_occurrences(doi = doi, file = "test_doi")
  expect_true(any(list.files(directory) == "test_doi.zip"))
  occ4 <- request_data() |>
    filter(doi == doi) |>
    collect(file = "test_doi2")
  expect_equal(occ3, occ4)
  expect_true(any(list.files(directory) == "test_doi2.zip"))
  # clean up
  unlink("TEMP", recursive = TRUE)
  cache_dir <- tempfile()
  dir.create(cache_dir)
  galah_config(directory = cache_dir)
  rm(cache_dir)
})

test_that("atlas_occurrences() errors with an invalid DOI", {
  expect_error(atlas_occurrences(doi = "random_doi"))
})

test_that("atlas_occurrences downloads data from a DOI", {
  skip_if_offline(); skip_on_ci()
  doi <- "10.26197/ala.0c1e8744-a639-47f1-9a5f-5610017ba060"
  result1 <- atlas_occurrences(doi = doi)
  expect_s3_class(result1, c("tbl_df", "tbl", "data.frame" ))
  expect_equal(nrow(result1), 9)
  expect_equal(ncol(result1), 68)
  # and with other syntax
  result2 <- request_data() |>
    filter(doi == doi) |>
    collapse()
  expect_equal(length(result2), 4)
  expect_s3_class(result2, "query")
  expect_equal(result2$type, "data/occurrences-doi")
  result3 <- collect(result2)
  expect_equal(result1, result3)
  # TODO add file name tests
})

test_that("`atlas_occurrences()` places DOI in `attr()` correctly", {
  skip_if_offline(); skip_on_ci()
  directory <- "TEMP"
  unlink(directory, recursive = TRUE)
  dir.create(directory)
  galah_config(directory = directory)
  x <- galah_call() |>
    identify("Vulpes vulpes") |>
    filter(year <= 1900, 
           basisOfRecord == "PRESERVED_SPECIMEN") |>
    collect(mint_doi = TRUE)
  y <- attr(x, "doi")
  expect_false(is.null(y))
  expect_true(grepl("^https://doi.org/", y))
  rm(x, y)
  # ditto for atlas_occurrences
  x <- galah_call() |>
    identify("Vulpes vulpes") |>
    filter(year <= 1900, 
           basisOfRecord == "PRESERVED_SPECIMEN") |>
    atlas_occurrences(mint_doi = TRUE)
  y <- attr(x, "doi")
  expect_false(is.null(y))
  expect_true(grepl("^https://doi.org/", y))
  unlink(directory, recursive = TRUE)
  cache_dir <- tempfile()
  dir.create(cache_dir)
  galah_config(directory = cache_dir)
  rm(cache_dir)
})

test_that("group_by works on occurrences", {
  skip_if_offline(); skip_on_ci()
  # compare group_by with atlas_species
  x <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia") |>
    group_by(speciesID) |>
    collect()
  y <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia") |>
    atlas_species()
  expect_equal(x, y)
  # try with a different variable
  z <- galah_call() |>
    filter(year == 2024,
           genus == "Crinia") |>
    group_by(genusID) |>
    collect()
  expect_true(inherits(z, c("tbl_df", "tbl", "data.frame")))
  expect_equal(colnames(z)[1], "taxon_concept_id")
})

test_that("atlas_occurrences() doesn't return secret information", {
  skip_if_offline(); skip_on_ci()
  RUN <- FALSE
  skip_if((!file.exists("testdata/SECRETS.txt") | !RUN),
          message = "Secret information not provided")
  data_request <- galah_call() |>
    identify("Acacia aneura") |>
    apply_profile(ALA) |>
    select(geodeticDatum, 
           eventRemarks,
           sensitive,
           dataGeneralizations,
           generalisationInMetres,
           coordinateUncertaintyInMeters,
           recordedBy,
           datePrecision,
           stateProvince,
           group = "basic")
  x_start <- collapse(data_request)
  # option to add tests here to ensure url is correctly constructed
  x <- collect(x_start)
  # import email address from SECRETS and check again
  # NOTE: SECRETS.txt has been added to .gitignore
  # It should never be placed on GitHub
  email <- readLines("testdata/SECRETS.txt", warn = FALSE)
  galah_config(email = email)
  y <- collect(data_request)
  expect_equal(nrow(x), nrow(y))
  expect_equal(ncol(x), ncol(y))
  expect_equal(colnames(x), colnames(y))
  expect_equal(x, y)
  # reset
  galah_config(email = "ala4r@ala.org.au")
})
