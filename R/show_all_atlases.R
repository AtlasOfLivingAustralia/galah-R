#' @export show_all_atlases
#' @rdname show_all_minifunctions
show_all_atlases <- function() {node_metadata}

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

## internal functions
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

# service_name <- function(url) {
#   switch (url,
#           data_quality_base_url = "Data quality filtering",
#           images_base_url = "Image downloading",
#           species_base_url = "Species information",
#           logger_base_url = "Logger service"
#   )
# }

species_facets <- function(){
  atlas <- getOption("galah_config")$atlas
  switch(atlas,
    # Australia = "speciesID",
    Austria = "species_guid",
    "speciesID"
  )
}