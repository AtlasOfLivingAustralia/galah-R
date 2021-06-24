server_config <- function(country) {
  conf <- switch (country,
                  "Australia" = aus_config(),
                  "UK" = uk_config(),
                  "Sweden" = sweden_config(),
                  "Austria" = austria_config(),
                  "Guatemala" = guatemala_config(),
                  "Spain" = spain_config()
  )
  # Possibly should go one step further and just have this info in a function- 
  # why bother storing in options?
  options(galah_server_config = conf)
}

aus_config <- function() {
  list(
    support_email = "support@ala.org.au", ## contact email
    spatial_base_url = "https://spatial.ala.org.au/ws/",
    species_base_url = "https://bie-ws.ala.org.au/ws",
    name_matching_base_url = "https://namematching-ws.ala.org.au/",
    records_base_url = "https://biocache-ws.ala.org.au/ws",
    data_quality_base_url = "https://data-quality-service.ala.org.au",
    doi_base_url = "https://https://doi.ala.org.au",
    images_base_url = "https://images.ala.org.au/",
    logger_base_url = "https://logger.ala.org.au/"
  )
}

sweden_config <- function() {
  list(
    # Uses GBIF taxonomy
    spatial_base_url = "https://spatial.biodiversitydata.se/ws/",
    species_base_url = "https://species.biodiversitydata.se/ws/",
    records_base_url = "https://records.biodiversitydata.se/ws/",
    images_base_url = "https://images.biodiversitydata.se/"
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
    images_base_url = "https://images.biodiversityatlas.at/"
  )
}

guatemala_config <- function() {
  # Uses GBIF taxonomy
  list(
    images_base_url = "https://imagenes.snib.conap.gob.gt/",
    spatial_base_url = "https://geoespacial.snib.conap.gob.gt/",
    records_base_url = "https://snib.conap.gob.gt/registros-ws/"
    # No species pages available
  )
}

spain_config <- function() {
  # Uses GBIF taxonomy
  list(
    records_base_url = "https://registros-ws.gbif.es/",
    species_base_url = "https://especies-ws.gbif.es/"
    
  )
}

base_config <- function() {
  list(
    species_base_url = "",
    records_base_url = "",
    spatial_base_url = "",
    images_base_url = ""
  )
}

supported_atlases <- function() {
  c("Australia", "UK", "Sweden", "Austria", "Guatemala", "Spain")
}
