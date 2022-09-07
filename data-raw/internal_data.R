# This script builds all information stored within galah/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by 
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

library(readr)
library(usethis)
library(dplyr)

# table of information on supported atlases
# placed here as hard-coded for galah 
node_metadata <- read_csv("./data-raw/node_metadata.csv") |>
  filter(supported == TRUE) |>
  select(-supported)
  
# configuration for web services of all atlases
# NOTE:
  # Australia, Brazil & UK use their own taxonomy
  # All other atlases use GBIF taxonomy
  # Order of priority is local-namematching > local-species > GBIF-namematching
  # France and Canada use GBIF due to lack of species service
node_config <- read_csv("./data-raw/node_config.csv")

# cached versions of some show_all functions
# NOTE: may be necessaary to expand this given changes to show_all
stored_functions <- c(
  "show_all_fields", "show_all_profiles", "show_all_reasons")

galah_internal_archived <- lapply(
  stored_functions,
  function(a){
    result <- eval(parse(text = paste0(a, "()")))
    attr(result, "ARCHIVED") <- TRUE
    attr(result, "atlas_name") <- "Australia"
    result
  })
# lapply(galah_internal_archived, attributes) # check
names(galah_internal_archived) <- stored_functions

# add to r/sysdata.rda
use_data(
  node_metadata,
  node_config,
  galah_internal_archived,
  internal = TRUE, 
  overwrite = TRUE)


## GBIF support
# there is no API for GBIF fields, so best practice is to store as local 
# csvs and import as per above

# tibble of available fields: 
  # sort(rgbif::occ_fields)
# tibble of assertions:
  # https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html