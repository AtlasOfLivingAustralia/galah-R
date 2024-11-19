test_that("`galah_select()` doesn't return error when columns don't exist", {
  expect_no_error(galah_select(basisOfRecord))
  expect_no_error(galah_select(year, basisOfRecord, eventdate))
})

test_that("`galah_select()` triggers error during `compute()` when columns don't exist", {
  skip_if_offline()
  expect_error(
    galah_call() |>
      identify("perameles") |>
      filter(year == 2003) |>
      galah_select(basisOfRecors) |>
      compute())
  expect_error(
    galah_call() |>
      identify("perameles") |>
      filter(year == 2003) |>
      select(year, basisOfRecors, eventdate) |>
      compute())
})

test_that("`galah_select()` returns requested columns", {
  skip_if_offline()
  galah_config(atlas = "Australia",
               email = "ala4r@ala.org.au", 
               run_checks = FALSE)
  query <- galah_call() |>
    identify("oxyopes dingo") |>
    select(year, basisOfRecord) |>
    atlas_occurrences()
  expect_s3_class(query, c("tbl_df", "tbl", "data.frame" ))
  expect_equal(names(query), c("year", "basisOfRecord"))
  expect_gte(nrow(query), 10)
})

test_that("`galah_select()` returns requested columns when piped", {
  skip_if_offline()
  galah_config(email = "ala4r@ala.org.au", run_checks = FALSE)
  query <- galah_call() |>
    identify("oxyopes dingo") |>
    select(year, basisOfRecord) |>
    collect()
  expect_s3_class(query, c("tbl_df", "tbl", "data.frame" ))
  expect_equal(names(query), c("year", "basisOfRecord"))
  expect_gte(nrow(query), 10)
})

test_that("`galah_select()` builds expected columns when group = basic", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "basic") |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], preset_groups("basic"))
  expect_equal(y$qa, "none")
})

test_that("`galah_select()` builds expected columns when group = event", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "event") |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               c("recordID", preset_groups("event")))
  expect_equal(y$qa, "none")
})

test_that("`galah_select()` accepts multiple groups", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = c("basic", "assertions")) |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               preset_groups("basic"))
  expect_equal(y$qa, "includeall")
})

test_that("galah_select defaults to group = 'basic' when there are no args", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], preset_groups("basic"))
  expect_equal(y$qa, "none")
})

test_that("galah_select works with group = 'taxonomy'", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "taxonomy") |>
    collapse()
  y <- url_parse(x$url)$query
  fields <- strsplit(tolower(y$fields), ",")[[1]]
  expect_equal(fields,
               c("recordid",
                 "kingdom",
                 "phylum", 
                 "class",
                 "order",
                 "family",
                 "genus",
                 "species",
                 "subspecies"))
})

test_that("galah_select returns assertions + recordID when group = assertions", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "assertions") |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(y$fields, "recordID")
  expect_equal(y$qa, "includeall")
})

test_that("galah_select combines requested columns and group columns", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(year, basisOfRecord, group = "basic") |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               c(preset_groups("basic"), "year", "basisOfRecord"))
})

test_that("galah_select can use tidyselect::contains", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::contains("el")) |>
    collapse()
  y <- url_parse(x$url)$query
  fields <- strsplit(tolower(y$fields), ",")[[1]]
  assertions <- strsplit(tolower(y$qa), ",")[[1]]
  expect_true(all(grepl("el", fields)))
  expect_true(all(grepl("el", assertions)))
})

test_that("galah_select can use tidyselect::starts_with", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::starts_with("el")) |>
    collapse()
  y <- url_parse(x$url)$query
  fields <- strsplit(tolower(y$fields), ",")[[1]]
  assertions <- strsplit(tolower(y$qa), ",")[[1]]
  expect_true(all(grepl("^el", fields)))
  expect_true(all(grepl("^el", assertions)))
})

test_that("galah_select can use tidyselect::last_col", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::last_col()) |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(y$fields, "recordID")
  expect_equal(y$qa, "ZERO_COORDINATE")
})

test_that("galah_select can use tidyselect::last_col & user-defined queries", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(year, basisOfRecord, tidyselect::last_col()) |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(y$fields, "year,basisOfRecord")
  expect_equal(y$qa, "ZERO_COORDINATE")
})

test_that("galah_select can use tidyselect::last_col & group", {
  skip_if_offline()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::last_col(), group = "basic") |>
    collapse()
  y <- url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               preset_groups("basic"))
  expect_equal(y$qa, "ZERO_COORDINATE")
})

test_that("galah_select warns for invalid field names when type = 'species'", {
  skip_if_offline()
  expect_warning({galah_call(type = "species") |>
    identify("Crinia") |>
    select(an_unrecognised_field_name) |>
    collapse()})
})

test_that("galah_select works for type = 'species' with no arguments", {
  skip_if_offline()
  x <- galah_call(type = "species") |>
    identify("Crinia") |>
    select() |>
    collect()
  expect_equal(colnames(x), "taxon_concept_id")
  expect_gt(nrow(x), 10)
})

test_that("galah_select works for type = 'species'", {
  skip_if_offline()
  x <- galah_call(type = "species") |>
      identify("Crinia") |>
      select(counts) |>
      collect()
  expect_equal(colnames(x), c("taxon_concept_id", "count"))
  expect_gt(nrow(x), 10)
})

test_that("galah_select works for type = 'species' with group = 'taxonomy'", {
  skip_if_offline()
  x <- galah_call(type = "species") |>
    identify("Crinia") |>
    select(counts, lists, group = "taxonomy") |>
    collect()
  expect_true(all(c("taxon_concept_id", "count", "kingdom", "phylum") %in% colnames(x)))
  expect_gt(nrow(x), 10)
})