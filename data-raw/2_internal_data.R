# This script builds all information stored within galah/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by 
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

## USE A TEST SYSTEM ##
# set to TRUE to run against respective ALA 'test' system
test_biocache = FALSE
test_lists = FALSE
# default is FALSE, which runs against the 'production' system.

devtools::load_all()
galah_config(atlas = "Australia")
library(readr) # import csvs straight to tibble
library(tibble) # generate tibbles
library(dplyr) # data manipulation
library(usethis) # adding content to sysdata.rda

# SET APIS
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
node_config <- read_csv("./data-raw/node_config.csv") |> 
  filter(atlas %in% node_metadata$region,
         functional == TRUE) |>
  select(-functional)

# if running on test server, reset requisite APIs
use_test_system <- function(biocache = FALSE,
                            lists = FALSE) {
  if(biocache == TRUE) {
    lookup <- grepl("^https://biocache-ws.ala.org.au/ws/", node_config$url)
    node_config$url[lookup] <- sub(
      "^https://biocache-ws.ala.org.au/ws/",
      "https://biocache-ws-test.ala.org.au/ws/",
      node_config$url[lookup])
  }
  
  if(lists == TRUE) {
    lookup <- grepl("^https://api.ala.org.au/specieslist/", node_config$url)
    node_config$url[lookup] <- sub(
      "^https://api.ala.org.au/specieslist/",
      "https://api.test.ala.org.au/specieslist/",
      node_config$url[lookup])
  }
  
  return(node_config)
}

node_config <- use_test_system(biocache = test_biocache,
                               lists = test_lists)

# ALA defaults
# add other data
galah_internal_archived <- list(
  ranks = tibble(
    id = seq_len(69),
    name = c("root", "superkingdom", "kingdom", "subkingdom",
             "superphylum", "phylum", "subphylum", "superclass", "class", 
             "subclass", "infraclass", "subinfraclass", 
             "superdivison zoology", "division zoology", 
             "subdivision zoology", "supercohort", "cohort", "subcohort", 
             "superorder", "order", "suborder", "infraorder", "parvorder", 
             "superseries zoology", "series zoology", "subseries zoology", 
             "supersection zoology", "section zoology", 
             "subsection zoology", "superfamily", "family", "subfamily", 
             "infrafamily", "supertribe", "tribe", "subtribe", 
             "supergenus", "genus group", "genus", "nothogenus", 
             "subgenus", "supersection botany", "section botany", 
             "subsection botany", "superseries botany", "series botany", 
             "subseries botany", "species group", "superspecies", 
             "species subgroup", "species", "nothospecies", "holomorph", 
             "anamorph", "teleomorph", "subspecies", "nothosubspecies", 
             "infraspecies", "variety", "nothovariety", "subvariety", 
             "form", "nothoform", "subform", "biovar", "serovar", 
             "cultivar", "pathovar", "infraspecific")),
  media_fields = tibble(
    id = c("multimedia", "images", "videos", "sounds"),
    description = "Media filter field",
    type = "media"),
  other_fields = tibble(
    id = c("qid", 
           "lsid", 
           "recordID",
           "species_list_uid"), 
    description = c("Reference to pre-generated query", 
                    "Left-side identifier", 
                    "Record identifier (galah internal)",
                    "Unique identifier for species lists"),
    type = "other"))

# cached versions of some show_all functions
# NOTE: may be necessary to expand this given changes to `show_all()`
stored_types <- c("assertions", "fields", "profiles", "reasons")
galah_internal_cached <- lapply(
  stored_types,
  function(a){
    result <- request_metadata(type = a) |> collect()
    attr(result, "ARCHIVED") <- TRUE
    attr(result, "region") <- "Australia"
    result
  })
# lapply(galah_internal_cached, attributes) # check
names(galah_internal_cached) <- stored_types

# Import web-scraped gbif data as csv
gbif_internal_archived <- list(
  assertions = read_csv("./data-raw/gbif_assertions.csv"),
  fields = read_csv("./data-raw/gbif_fields.csv"),
  ranks =  tibble(
    id = seq_len(9),
    name = c("kingdom", "phylum", "class", 
             "order", "family", "genus", 
             "species", "subspecies", "infraspecific")))

# add to r/sysdata.rda
use_data(
  node_metadata,
  node_config,
  galah_internal_archived,
  galah_internal_cached,
  gbif_internal_archived,
  internal = TRUE, 
  overwrite = TRUE)
