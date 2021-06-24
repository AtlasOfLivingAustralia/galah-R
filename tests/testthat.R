library(testthat)
library(galah)

ala_config(cache_directory = tempdir(), email = "ala4r@ala.org.au",
           caching = FALSE, country = "Australia")
test_check("galah")
