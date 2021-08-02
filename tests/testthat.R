library(testthat)
library(galah)

galah_config(cache_directory = tempdir(), email = "ala4r@ala.org.au",
           caching = FALSE, atlas = "Australia", verbose = FALSE,
           run_checks = TRUE)
test_check("galah")
