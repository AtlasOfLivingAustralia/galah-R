library(testthat)
library(galah)
# library(httptest2)

galah_config(cache_directory = tempdir(), email = "ala4r@ala.org.au",
           caching = FALSE, atlas = "Australia", verbose = FALSE,
           run_checks = TRUE)
# httptest2::start_capturing()
test_check("galah")
# httptest2::stop_capturing()