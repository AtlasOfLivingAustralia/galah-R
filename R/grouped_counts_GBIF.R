
# workhorse function to do most of the actual processing
## NOTE: need to turn off caching for multiple runs
grouped_counts_GBIF <- function(identify = NULL, 
                        filter = NULL, 
                        geolocate = NULL,
                        profile = NULL,
                        facets, # NOTE: not `groups` as no multiply section here
                        limit = NULL, 
                        type = "record",
                        refresh_cache = FALSE,
                        verbose = FALSE # NOTE: internally `verbose` is manual, not from galah_config
) {

  # excluded args
  if(!is.null(geolocate)){
    inform("`geolocate` is not supported for the selected atlas")
    geolocate <- NULL
  }
  
  # build query
  query <- list()
  query <- build_query(identify, filter, geolocate)
  
  # add facets in a way that supports single or multiple queries 
  facets_temp <- as.list(facets)
  names(facets_temp) <- rep("facet", length(facets_temp))
  query <- c(query, 
             facets_temp, 
             limit = 0, 
             facetLimit = 100)
  
  url <- url_lookup("records_counts")
  resp <- url_GET(url, params = query)

  if(is.null(resp)){
    return(NULL)
  }else{
    result <- resp$facets$counts |> 
      bind_rows() |> 
      tibble()
    colnames(result)[seq_along(facets)] <- facets
    return(result)
  }

}