#' List supported Living Atlases
#' 
#' galah supports downloading data from a number of International Living
#' Atlases. Use this function to get a list of all currently supported atlases.
#' @seealso This function is helpful in setting up \code{\link{galah_config}()}.
#' @return a \code{data.frame} of Living Atlas information, including taxonomy
#' source and information for each atlas.
#' @export
find_atlases <- function() {
  gbif_info <- "https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  ala_info <- "https://bie.ala.org.au/"
  nbn_info <- "https://www.nhm.ac.uk/our-science/data/uk-species.html"
  return(
    data.frame(
      atlas = c("Australia", "Austria", "Guatemala", "Spain", "Sweden", "UK"),
      taxonomy_source = c("ALA", "GBIF", "GBIF", "GBIF", "GBIF", "NBN"),
      taxonomy_info  = c(ala_info, gbif_info, gbif_info, gbif_info, gbif_info,
                         nbn_info)
    )
  )
}

server_config <- function(url) {
  atlas <- getOption("galah_config")$atlas
  conf <- switch (atlas,
                  "Australia" = aus_config(),
                  "UK" = uk_config(),
                  "Sweden" = sweden_config(),
                  "Austria" = austria_config(),
                  "Guatemala" = guatemala_config(),
                  "Spain" = spain_config()
  )
  if (url == "records_download_base_url" & !url %in% names(conf)) {
    url <- "records_base_url"
  }
  if (!(url %in% names(conf))) {
    stop(service_name(url), " is not supported for the ", atlas,
         " atlas.")
  }
  return(conf[[url]])
}


image_fields <- function() {
  atlas <- getOption("galah_config")$atlas
  switch (atlas,
          "Austria" = "all_image_url",
          "Guatemala" = "all_image_url",
          "Spain" = "all_image_url",
          c("images", "videos", "sounds")
  )
}

default_columns <- function() {
  atlas <- getOption("galah_config")$atlas
  switch (atlas,
          "Guatemala" = c("latitude", "longtitude", "species_guid",
                          "data_resource_uid", "occurrence_date", "id"),
          c("decimalLatitude", "decimalLongitude", "eventDate",
            "scientificName", "taxonConceptID", "recordID", "dataResourceName")
  )
}

service_name <- function(url) {
  switch (url,
          data_quality_base_url = "Data quality filtering",
          images_base_url = "Image downloading",
          species_base_url = "Species information"
  )
}

aus_config <- function() {
  list(
    support_email = "support@ala.org.au", ## contact email
    spatial_base_url = "https://spatial.ala.org.au/ws/",
    species_base_url = "https://bie-ws.ala.org.au/ws",
    name_matching_base_url = "https://namematching-ws.ala.org.au/",
    records_base_url = "https://biocache-ws.ala.org.au/ws",
    # this is only different in aus
    records_download_base_url = "https://biocache-ws.ala.org.au/",
    data_quality_base_url = "https://data-quality-service.ala.org.au",
    doi_base_url = "https://doi.ala.org.au",
    images_base_url = "https://images.ala.org.au/",
    logger_base_url = "https://logger.ala.org.au/"
  )
}

sweden_config <- function() {
  list(
    # Uses GBIF taxonomy
    spatial_base_url = "https://spatial.biodiversitydata.se/ws/",
    species_base_url = "https://species.biodiversitydata.se/ws/",
    records_base_url = "https://records.biodiversitydata.se/ws/"
  )
}

uk_config <- function() {
  list(
    # Uses NBN taxonomy
    spatial_base_url = "https://layers.nbnatlas.org/ws",
    species_base_url = "https://species-ws.nbnatlas.org",
    records_base_url = "https://records-ws.nbnatlas.org",
    images_base_url = "https://images.nbnatlas.org/"
  )
}

austria_config <- function() {
  # Uses GBIF taxonomy
  list(
    species_base_url = "https://bie-ws.biodiversityatlas.at/",
    records_base_url = "https://biocache-ws.biodiversityatlas.at/",
    spatial_base_url = "https://spatial.biodiversityatlas.at/ws",
    # Occurrence download only returns the first image
    images_base_url = "https://images.biodiversityatlas.at/"
  )
}

guatemala_config <- function() {
  # Uses GBIF taxonomy
  list(
    images_base_url = "https://imagenes.snib.conap.gob.gt/",
    spatial_base_url = "https://geoespacial.snib.conap.gob.gt/ws",
    records_base_url = "https://snib.conap.gob.gt/registros-ws/"
    # No species pages available
  )
}

spain_config <- function() {
  # Uses GBIF taxonomy
  list(
    records_base_url = "https://registros-ws.gbif.es/",
    species_base_url = "https://especies-ws.gbif.es/",
    images_base_url = "https://imagenes.gbif.es/",
    spatial_base_url = "https://espacial.gbif.es/ws",
    doi_base_url = "https://doi.gbif.es/",
    logger_base_url = "https://logger.gbif.es/"
  )
}

