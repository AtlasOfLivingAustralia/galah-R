# set up quiet functions for testing reasons
quiet_collect <- function(x){
  purrr_collect <- purrr::quietly(collect.data_request)
  purrr_collect(x) |> 
    purrr::pluck("result")
}
quiet_occurrences <- purrr::quietly(atlas_occurrences)

config_capture <- galah_config(email = "ala4r@ala.org.au",
                               run_checks = TRUE)

test_that("`select.data_request()` adds content to a `data_request` object", {
  x <- galah_call() |>
    select(group = "basic")
  expect_equal(names(x$select),
               c("quosure", "summary", "group"))
})

test_that("`galah_select()` doesn't return error when columns don't exist", {
  galah_select(basisOfRecord) |>
    expect_no_error()
  galah_select(year, basisOfRecord, eventdate) |>
    expect_no_error()
})

test_that("`select()` triggers error during `collapse()` when columns don't exist", {
  skip_if_offline(); skip_on_ci()
  expect_error(
    galah_call() |>
      identify("perameles") |>
      filter(year == 2003) |>
      select(basisOfRecors) |>
      collapse())
  expect_error(
    galah_call() |>
      identify("perameles") |>
      filter(year == 2003) |>
      select(year, basisOfRecors, eventdate) |>
      collapse())
})

test_that("`select()` builds expected columns when group = basic", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "basic") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], preset_groups("basic"))
  expect_equal(y$qa, "none")
})

test_that("`select()` builds expected columns when group = event", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "event") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               c("recordID", preset_groups("event")))
  expect_equal(y$qa, "none")
})

test_that("`select()` accepts multiple groups", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = c("basic", "assertions")) |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               preset_groups("basic"))
  expect_equal(y$qa, "includeall")
})

test_that("`select()` defaults to group = 'basic' when there are no args", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], preset_groups("basic"))
  expect_equal(y$qa, "none")
})

test_that("`select()` works with group = 'taxonomy'", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "taxonomy") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
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

test_that("`select()` returns assertions + recordID when group = assertions", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(group = "assertions") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(y$fields, "recordID")
  expect_equal(y$qa, "includeall")
})

test_that("`select()` combines requested columns and group columns", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(year, group = "basic") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               c(preset_groups("basic"), "year"))
})

test_that("`select()` can use `tidyselect::contains()`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::contains("el")) |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  fields <- strsplit(tolower(y$fields), ",")[[1]]
  assertions <- strsplit(tolower(y$qa), ",")[[1]]
  expect_true(all(grepl("el", fields)))
  expect_true(all(grepl("el", assertions)))
})

# TODO: `galah_select()` fails when `everything()` is called for `type = "occurrences"`

test_that("`select()` can use `tidyselect::starts_with()`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::starts_with("el")) |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  fields <- strsplit(tolower(y$fields), ",")[[1]]
  assertions <- strsplit(tolower(y$qa), ",")[[1]]
  expect_true(all(grepl("^el", fields)))
  expect_true(all(grepl("^el", assertions)))
})

test_that("`select()` can use `tidyselect::last_col()`", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::last_col()) |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(y$fields, "recordID")
  expect_equal(y$qa, "ZERO_COORDINATE")
})

test_that("`select()` can use `tidyselect::last_col()` & user-defined queries", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(year, basisOfRecord, tidyselect::last_col()) |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(y$fields, "year,basisOfRecord")
  expect_equal(y$qa, "ZERO_COORDINATE")
})

test_that("`select()` can use `tidyselect::last_col()` & group", {
  skip_if_offline(); skip_on_ci()
  x <- galah_call() |>
    identify("oxyopes dingo") |>
    select(tidyselect::last_col(), group = "basic") |>
    collapse()
  y <- httr2::url_parse(x$url)$query
  expect_equal(strsplit(y$fields, ",")[[1]], 
               preset_groups("basic"))
  expect_equal(y$qa, "ZERO_COORDINATE")
})

test_that("`select()` warns for invalid field names when type = 'species'", {
  skip_if_offline(); skip_on_ci()
  expect_warning({galah_call() |>
    identify("Crinia") |>
    group_by(speciesID) |>
    select(an_unrecognised_field_name) |>
    as_query()})
})

rm(quiet_collect, quiet_occurrences, config_capture)
