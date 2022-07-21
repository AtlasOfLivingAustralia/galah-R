# This script builds all information stored within galah/R/sysdata.rda
# storing of such code in /data-raw is recommended in 'R Packages' by 
# Hadley Wickham, section 8.3 'Internal data'
# https://r-pkgs.org/data.html

# show_all_atlases
all_atlas_metadata <- tibble(
  atlas = c(
    "Australia",
    "Austria",
    "Brazil",
    "Canada",
    "Estonia",
    "France",
    # "Global",
    "Guatemala",
    "Portugal",
    "Spain", 
    "Sweden", 
    "UK",
    "Vermont"
  ),
  institution = c(
    "Atlas of Living Australia",
    "Biodiversitäts-Atlas Österreich",
    "Sistemas de Informações sobre a Biodiversidade Brasileira",
    "Candensys",
    "eElurikkus",
    "Inventaire National du Patrimoine Naturel",
    # "Global Biodiversity Information Facility",
    "Sistema Nacional de Información sobre Diversidad Biológica de Guatemala",
    "GBIF Portugal",
    "GBIF Spain",
    "Swedish Biodiversity Data Infrastructure",
    "National Biodiversity Network",
    "Vermont Atlas of Life"
  ),
  acronym = c(
    "ALA", 
    "BAO",
    "SiBBr",
    NA,
    NA,
    "INPN",
    # "GBIF",
    "SNIBgt",
    "GBIF.pt",
    "GBIF.es", 
    "SBDI", 
    "NBN",
    "VAL"
  ),
  url = c(
    "https://www.ala.org.au",
    "https://biodiversityatlas.at",
    "https://sibbr.gov.br",
    "http://www.canadensys.net",
    "https://elurikkus.ee",
    "https://inpn.mnhn.fr",
    # "https://gbif.org",
    "https://snib.conap.gob.gt",
    "https://www.gbif.pt",
    "https://www.gbif.es",
    "https://biodiversitydata.se",
    "https://nbn.org.uk",
    "https://val.vtecostudies.org"
  )
)


# configuration for web services of all atlases
# NOTE:
  # Australia, Brazil & UK use their own taxonomy
  # All other atlases use GBIF taxonomy
  # Order of priority is local-namematching > local-species > GBIF-namematching
  # France and Canada use GBIF due to lack of species service
all_atlas_config <- list(
  Australia = list(
    support_email = "support@ala.org.au", ## contact email
    spatial_base_url = "https://spatial.ala.org.au/ws/",
    species_base_url = "https://bie-ws.ala.org.au/ws",
    name_matching_base_url = "https://namematching-ws.ala.org.au/",
    records_base_url = "https://biocache-ws.ala.org.au/ws",
    collections_base_url = "https://collections.ala.org.au/ws/",
    # this is only different in aus
    records_download_base_url = "https://biocache-ws.ala.org.au/",
    data_quality_base_url = "https://data-quality-service.ala.org.au",
    doi_base_url = "https://doi.ala.org.au",
    images_base_url = "https://images.ala.org.au/ws/",
    logger_base_url = "https://logger.ala.org.au/",
    lists_base_url = "https://lists.ala.org.au/ws/"
  ),
  Austria = list(
    species_base_url = "https://bie-ws.biodiversityatlas.at/",
    records_base_url = "https://biocache-ws.biodiversityatlas.at",
    spatial_base_url = "https://spatial.biodiversityatlas.at/ws",
    # Occurrence download only returns the first image
    images_base_url = "https://images.biodiversityatlas.at/",
    collections_base_url = "https://collectory.biodiversityatlas.at/ws/",
    logger_base_url = "https://logger.biodiversityatlas.at/",
    lists_base_url = "https://lists.biodiversityatlas.at/"
  ),
  Brazil = list(
    species_base_url = "https://bie-webservice.sibbr.gov.br/bie-index/",
    records_base_url = "https://biocache-service.sibbr.gov.br/biocache-service/",
    spatial_base_url = "https://portal-espacial.sibbr.gov.br/spatial-hub/"
  ),
  Canada = list(
    species_base_url = "https://api.gbif.org/v1",
    records_base_url = "http://explorer-ws.canadensys.net/",
    collections_base_url = "http://collections.canadensys.net/"   
  ),
  Estonia = list(
    species_base_url = "https://elurikkus.ee/bie-index/",
    records_base_url = "https://elurikkus.ee/biocache-service/"
  ),
  France = list(
    species_base_url = "https://api.gbif.org/v1",
    records_base_url = "http://portail.gbif.fr/biocache-service/"
  ),
  # Global = list(
  #   species_base_url = "https://api.gbif.org/v1",
  #   records_base_url = "https://api.gbif.org/v1"
  # ),
  Guatemala = list(
    species_url = "https://snib.conap.gob.gt/especies-ws",
    records_base_url = "https://snib.conap.gob.gt/registros-ws/",
    images_base_url = "https://imagenes.snib.conap.gob.gt/",
    spatial_base_url = "https://geoespacial.snib.conap.gob.gt/ws"
  ),
  Spain = list(
    records_base_url = "https://registros-ws.gbif.es/",
    species_base_url = "https://listas.gbif.es/",
    images_base_url = "https://imagenes.gbif.es/",
    spatial_base_url = "https://espacial.gbif.es/",
    doi_base_url = "https://doi.gbif.es/",
    logger_base_url = "https://logger.gbif.es/"
  ),
  Sweden = list(
    species_base_url = "https://species.biodiversitydata.se/ws/",
    records_base_url = "https://records.biodiversitydata.se/ws/",
    spatial_base_url = "https://spatial.biodiversitydata.se/"
  ),
  UK = list(
    species_base_url = "https://species-ws.nbnatlas.org",
    records_base_url = "https://records-ws.nbnatlas.org",
    spatial_base_url = "https://spatial.nbnatlas.org/",
    images_base_url = "https://images.nbnatlas.org/"   
  ),
  Vermont = list(
    species_base_url = "https://bie-ws.vtatlasoflife.org/",
    records_base_url = "https://biocache-ws.vtatlasoflife.org/"
  )
)


# user-visible lookup tables 
# NOTE: "show_all_ranks"  and "show_all_atlases" are not included,
# as they don't query a web service
devtools::load_all()
stored_functions <- c(
  "show_all_fields", "show_all_profiles", "show_all_reasons")
# load all data
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
usethis::use_data(
  galah_internal_archived,
  all_atlas_metadata,
  all_atlas_config,
  internal = TRUE, 
  overwrite = TRUE)


# # there is no API for GBIF fields, so hard-code them
# # this list can be found using sort(rgbif::occ_fields)
# # NOTE: these are not currently used anywhere in `galah`
# gbif_fields <- function(){
#   data.frame(id = c(
#     "basisOfRecord", "catalogNumber", "class", "classKey",       
#     "collectionCode", "country", "countryCode", "datasetKey",
#     "datasetName", "dateIdentified", "day", "decimalLatitude",
#     "decimalLongitude", "eventDate", "eventTime", "extensions",
#     "facts", "family", "familyKey", "gbifID", "genericName",
#     "genus", "genusKey", "geodeticDatum", "identificationID",
#     "identifier", "identifiers", "institutionCode", "issues",
#     "key", "kingdom", "kingdomKey", "lastCrawled", 
#     "lastInterpreted", "lastParsed", "modified", "month",
#     "name", "occurrenceRemarks", "order", "orderKey",
#     "phylum", "phylumKey", "protocol", "publishingCountry",
#     "publishingOrgKey", "recordedBy", "references", "relations",
#     "rights", "rightsHolder", "scientificName", "species",
#     "speciesKey", "specificEpithet", "taxonID", "taxonKey",
#     "taxonRank", "verbatimEventDate", "year" 
#   ))
# }

# # https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
# gbif_assertions <- function() {
#   tibble(
#     id = c(
#       "AMBIGUOUS_COLLECTION",
#       "AMBIGUOUS_INSTITUTION",
#       "BASIS_OF_RECORD_INVALID"
#     ),
#     description = c(
#       "The given collection matches with more than 1 GRSciColl collection",
#       "The given institution matches with more than 1 GRSciColl institution",
#       "The given basis of record is impossible to interpret or significantly different from the recommended vocabulary"
#     ),
#     type = "assertions"
#   )
# }