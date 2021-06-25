library(testthat)
library(galah)

ala_config(cache_directory = tempdir(), email = "ala4r@ala.org.au",
           caching = FALSE, country = "Australia", verbose = FALSE)
test_check("galah")
