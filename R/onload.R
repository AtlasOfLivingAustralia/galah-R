.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
        ## populate the options slot
        ala_config() ## will set to default values if not already set

        ## also populate the server configuration info
        server_config <- list(
            notify = "If this problem persists please notify the galah
            maintainers by lodging an issue at
            https://github.com/AtlasOfLivingAustralia/galah/issues/
            or emailing support@ala.org.au",
            support_email = "support@ala.org.au", ## contact email
            base_url_spatial = "https://spatial.ala.org.au/ws/",
            base_url_bie = "https://bie-ws.ala.org.au/",
            base_url_name_matching = "https://namematching-ws.ala.org.au/",
            base_url_biocache = "https://biocache-ws.ala.org.au/",
            base_url_data_quality = "https://data-quality-service.ala.org.au",
            base_url_doi = "https://https://doi.ala.org.au",
            base_url_images = "https://images.ala.org.au/",
            base_url_logger = "https://logger.ala.org.au/"
        )
        if (!"galah_server_config" %in% names(options())) {
            options(galah_server_config = server_config)
        }
    }
}
