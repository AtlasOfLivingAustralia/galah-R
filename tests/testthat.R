library(testthat)
library(ALA4R)

ala_config(cache_directory = tempdir(), email = "ala4r@ala.org.au",
           caching = FALSE)
test_check("ALA4R")
