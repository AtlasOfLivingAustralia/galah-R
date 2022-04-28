#' @export show_all_atlases
#' @rdname show_all_minifunctions
show_all_atlases <- function() {
  gbif_info <- "https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  ala_info <- "https://bie.ala.org.au/"
  nbn_info <- "https://www.nhm.ac.uk/our-science/data/uk-species.html"
  tibble(
    atlas = c("Australia", "Austria", "Global", "Guatemala", 
              "Spain", "Sweden", "UK"),
    acronym = c("ALA", "BAO", "GBIF", "SNIBgt",
                "GBIF.es", "SBDI", "NBN"),
    url = c(
      "https://www.ala.org.au",
      "https://biodiversityatlas.at",
      "https://gbif.org",
      "https://snib.conap.gob.gt",
      "https://www.gbif.es",
      "https://biodiversitydata.se",
      "https://nbn.org.uk"
    ),
    taxonomy_source = c("ALA", "GBIF", "GBIF", "GBIF", 
                        "GBIF", "GBIF", "NBN"),
    taxonomy_info  = c(ala_info, gbif_info, gbif_info, gbif_info, 
                       gbif_info, gbif_info, nbn_info)
  )
}

#' @rdname search_minifunctions
#' @export search_atlases
search_atlases <- function(query){
  df <- show_all_atlases()
  df[grepl(
    tolower(query), 
    tolower(apply(
      df[, c("acronym", "region")], 1, 
      function(a){paste(a, collapse = "-")})
    )
  ), ]
}

# internal functions
server_config <- function(url, error_call = caller_env()) {
  atlas <- getOption("galah_config")$atlas
  
  # set configuration
  conf <- switch(atlas,
                  "Australia" = aus_config(),
                  "Austria" = austria_config(),
                  "Global" = gbif_config(),
                  "Guatemala" = guatemala_config(),
                  "Spain" = spain_config(),
                  "Sweden" = sweden_config(),
                  "UK" = uk_config()
  )
  if (url == "records_download_base_url" & !url %in% names(conf)) {
    url <- "records_base_url"
  }
  if (!(url %in% names(conf))) {
    service <- service_name(url)
    lookup <- show_all_atlases()
    atlas_acronym <- lookup$acronym[lookup$atlas == atlas]
    abort(
      glue("{service} is not supported for {atlas_acronym}"),
      call = error_call)
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
          species_base_url = "Species information",
          logger_base_url = "Logger service"
  )
}


# config for specific services, in same order as show_all_atlases()
aus_config <- function() {
  list(
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
    images_base_url = "https://images.ala.org.au/",
    logger_base_url = "https://logger.ala.org.au/",
    lists_base_url = "https://lists.ala.org.au/ws/"
  )
}

austria_config <- function() {
  # Uses GBIF taxonomy
  list(
    species_base_url = "https://bie-ws.biodiversityatlas.at/",
    records_base_url = "https://biocache-ws.biodiversityatlas.at/",
    spatial_base_url = "https://spatial.biodiversityatlas.at/ws",
    # Occurrence download only returns the first image
    images_base_url = "https://images.biodiversityatlas.at/",
    name_matching_base_url = "https://api.gbif.org/v1"
  )
}

gbif_config <- function() {
  list(
    name_matching_base_url = "https://api.gbif.org/v1"
    # records_base_url = "https://api.gbif.org/v1/occurrence/"
  )
}

guatemala_config <- function() {
  # Uses GBIF taxonomy
  list(
    images_base_url = "https://imagenes.snib.conap.gob.gt/",
    spatial_base_url = "https://geoespacial.snib.conap.gob.gt/ws",
    records_base_url = "https://snib.conap.gob.gt/registros-ws/",
    name_matching_base_url = "https://api.gbif.org/v1"
    # No species pages available
  )
}

spain_config <- function() {
  # Uses GBIF taxonomy
  list(
    records_base_url = "https://registros-ws.gbif.es/",
    species_base_url = "https://especies-ws.gbif.es/",
    name_matching_base_url = "https://api.gbif.org/v1",
    images_base_url = "https://imagenes.gbif.es/",
    spatial_base_url = "https://espacial.gbif.es/ws",
    doi_base_url = "https://doi.gbif.es/",
    logger_base_url = "https://logger.gbif.es/"
  )
}

sweden_config <- function() {
  list(
    # Uses GBIF taxonomy
    spatial_base_url = "https://spatial.biodiversitydata.se/ws/",
    species_base_url = "https://species.biodiversitydata.se/ws/",
    records_base_url = "https://records.biodiversitydata.se/ws/",
    name_matching_base_url = "https://api.gbif.org/v1"
  )
}

uk_config <- function() {
  list(
    # Uses NBN taxonomy
    spatial_base_url = "https://layers.nbnatlas.org/ws",
    species_base_url = "https://species-ws.nbnatlas.org",
    records_base_url = "https://records-ws.nbnatlas.org",
    images_base_url = "https://images.nbnatlas.org/",
  )
}