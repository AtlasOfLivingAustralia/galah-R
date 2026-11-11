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
  line_exclusions = c("capture_events.R",
                      "collapse_events.R",
                      "collect_events.R"),
  allow_failures = TRUE)  # pessimism not idealism
covr::to_badge(x, "./man/figures/codecov.svg")
# covr::report(x) # to investigate locally
