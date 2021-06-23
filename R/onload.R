.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
        ## populate the options slot
        ala_config() ## will set to default values if not already set

        ## also populate the server configuration info
        server_config <- list(
            country = "Australia",
            spatial_base_url = "https://spatial.ala.org.au/ws/",
            species_base_url = "https://bie-ws.ala.org.au/ws",
            name_matching_base_url = "https://namematching-ws.ala.org.au/",
            records_base_url = "https://biocache-ws.ala.org.au/ws",
            data_quality_base_url = "https://data-quality-service.ala.org.au",
            doi_base_url = "https://https://doi.ala.org.au",
            images_base_url = "https://images.ala.org.au/",
            logger_base_url = "https://logger.ala.org.au/"
        )
        if (!"galah_server_config" %in% names(options())) {
            options(galah_server_config = server_config)
        }
    }
}
