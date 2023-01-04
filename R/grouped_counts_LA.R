
# workhorse function to do most of the actual processing
## NOTE: need to turn off caching for multiple runs
grouped_counts_LA <- function(identify = NULL, 
                                filter = NULL, 
                                geolocate = NULL,
                                profile = NULL,
                                facets, # NOTE: not `groups` as no multiply section here
                                limit = NULL, 
                                type = "record",
                                refresh_cache = FALSE,
                                verbose = FALSE # NOTE: internally `verbose` is manual, not from galah_config
) {
  page_size <- 100
  query <- list()
  query <- build_query(identify, filter, geolocate, profile = profile)
  
  # add facets in a way that supports single or multiple queries 
  facets_temp <- as.list(facets)
  names(facets_temp) <- rep("facets", length(facets_temp))
  query <- c(query, facets_temp)
  
  ## build url etc
  total_cats <- total_categories(query)[1]
  if(is.null(total_cats)) {
    return(NULL)
  }
  if(sum(total_cats) < 1){
    return(tibble(count = 0))
  }
  
  if (is.null(limit)) {
    limit <- sum(total_cats)
  }
  
  url <- url_lookup("records_facets")
  if (limit > page_size) {
    resp <- url_paginate(url, 
                         params = query, 
                         group_size = page_size,
                         limit = limit,
                         limit_name = "flimit",
                         offset_name = "foffset",
                         slot_name = "fieldResult")
  } else {
    query$flimit <- max(limit)
    resp <- url_GET(url, params = query)$fieldResult
  }
  
  if(is.null(resp)){return(NULL)}
  counts <- bind_rows(resp)
  
  if (sum(total_cats) > limit & galah_config()$package$verbose) {
    bullets <- c(
      glue("This field has {total_cats} values. {limit} will be returned."),
      i = "Increase `limit` to return more values, or decrease `limit` to return fewer."
    )
    inform(bullets) 
  }
  
  # parse out value
  value <- parse_fq(counts$fq)
  
  if (type == "species") {
    # this can take a while so add progress bar
    if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
    counts_final <- lapply(seq_along(value), function(x) {
      if (verbose) {
        val <- (x / length(value))
        setTxtProgressBar(pb, val)
      }
      species_query <- list()
      species_query$fq <- c(query$fq,
                            # query_term(name = facets, value = value[[x]], include = TRUE))
                            counts$fq[[x]])
      if(!is.null(geolocate)){
        species_query$wkt <- query$wkt
      }
      count <- species_count(species_query)
      if(is.null(count)){count <- NA}
      data.frame(name = value[[x]], count = count) |> as_tibble()
    }) |>
      bind_rows()
  } else {
    counts_final <- data.frame(
      name = value,
      count = counts$count)
  }
  
  if(length(facets) > 1){
    counts_final$field_name <- parse_field(counts$fq)
    counts_list <- split(counts_final, counts_final$field_name)
    counts_final <- lapply(
      seq_along(facets), function(a){
        names(counts_list[[a]])[1] <- names(counts_list)[a]
        counts_list[[a]]
      }) |>
      bind_rows()
    counts_final <- counts_final[, c(names(counts_list), "count")]
  }else{ # i.e. only one facet
    names(counts_final) <- c(facets, "count")
  }  
  
  tibble(counts_final)
}