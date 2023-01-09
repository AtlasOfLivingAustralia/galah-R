# This script builds all information stored within galah/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by 
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

devtools::load_all()
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
  
node_metadata$institution[
  which(node_metadata$acronym == "OpenObs")
  ] <- "Portail français d'accès aux données d'observation sur les espèces"

# configuration for web services of all atlases
# NOTE:
  # Australia, Brazil & UK use their own taxonomy
  # All other atlases use GBIF taxonomy
  # Order of priority is local-namematching > local-species > GBIF-namematching
  # France and Canada use GBIF due to lack of species service
node_config <- read_csv("./data-raw/node_config.csv") |> 
  filter(atlas %in% node_metadata$region)


# ALA defaults
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


# Import web-scraped gbif data as csv
gbif_internal_archived <- list(
  fields = read_csv("./data-raw/gbif_fields.csv"),
  assertions = read_csv("./data-raw/gbif_assertions.csv"))


# add to r/sysdata.rda
use_data(
  node_metadata,
  node_config,
  galah_internal_archived,
  gbif_internal_archived,
  internal = TRUE, 
  overwrite = TRUE)