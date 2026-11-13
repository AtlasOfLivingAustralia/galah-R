# simple script to build codecov badge
# this is useful because `covr` doesn't play well with `skip_on_cran()`,
# which is called by `skip_if_offline()`, which we use extensively in
# `galah`. This implements an alternative solution of rendering codecov
# scores locally, then building the badge.
library(covr)
library(devtools)
devtools::load_all()
Sys.setenv(NOT_CRAN = "true")
x <- covr::package_coverage(
  # remove unreleased functions
  line_exclusions = list(
    # event functions are not available yet
    "R/capture_events.R",
    "R/collapse_events.R",
    "R/collect_events.R",
    # authentication can't be tested except interactively, so will always
    # be skipped by the test suite. These tests exist, but can't
    # be counted by `covr`, so the corresponding code shouldn't either.
    "R/authenticate.R" = c(80:92),
    "R/query_API.R" = c(233:316)
  ),
  allow_failures = TRUE,  # pragmatism not idealism
  show_code_output = TRUE)
covr::to_badge(x, "./man/figures/codecov.svg")
covr::report(x) # to investigate locally
