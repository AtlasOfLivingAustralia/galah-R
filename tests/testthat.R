library(testthat)
library(koala)

ala_config(cache_directory = tempdir(), email = "ala4r@ala.org.au",
           caching = FALSE)
test_check("koala")
