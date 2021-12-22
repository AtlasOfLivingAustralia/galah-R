#' Return a count of records
#'
#' Prior to downloading data it is often valuable to have some estimate of how
#' many records are available, both for deciding if the query is feasible,
#' and for estimating how long it will take to download. Alternatively, for some kinds
#' of reporting, the count of observations may be all that is required, for example
#' for understanding how observations are growing or shrinking in particular
#' location of for particular taxa. To this end, `atlas_counts()` takes
#' arguments in the same format as [atlas_occurrences()], and
#' provides either a total count of records matching the criteria, or a
#' `data.frame` of counts matching the criteria supplied to the `group_by`
#' argument.
#'
#' @inheritParams atlas_occurrences
#' @param group_by `data.frame`: An object of class `galah_group_by`,
#' as returned by [galah_group_by()]. Alternatively a vector of field
#' names (see [search_fields()] and [show_all_fields()].
#' @param limit `numeric`: maximum number of categories to return, defaulting to 100.
#' If limit is NULL, all results are returned. For some categories this will
#' take a while.
#' @param type `string`: one of `c("record", "species")`. Defaults to
#' "record". If "species", the number of species matching the criteria will be
#' returned, if "record", the number of records matching the criteria will be
#' returned.
#' @param refresh_cache `logical`: if set to `TRUE` and 
#' `galah_config(caching = TRUE)` then files cached from a previous query will 
#' be replaced by the current query
#' @return
#' 
#' An object of class `tbl_df` and `data.frame` (aka a tibble) returning: 
#'  * A single number, if `group_by` is not specified or,
#'  * A summary of counts grouped by field(s), if `group_by` is specified
#'
#' @examples
#' # With no arguments, return the total number of records in the ALA
#' atlas_counts()
#'
#' # Group counts by state and territory
#' atlas_counts(group_by = galah_group_by(stateProvince))
#'
#' # Count records matching a filter
#' atlas_counts(filter = galah_filter(basisOfRecord == "FossilSpecimen"))
#' 
#' # Count the number of species recorded for each kingdom
#' atlas_counts(group_by = galah_group_by(kingdom), type = "species")
#' 
#' # Crosstabulate using two different variables
#' atlas_counts(
#'   filter = galah_filter(year > 2015),
#'   select = galah_select(year, basisOfRecord))
#' 
#' @export
atlas_counts <- function(...) {
  UseMethod("atlas_counts")
}

#' @export
#' @rdname atlas_counts
atlas_counts.data_request <- function(request, ...) {
  do.call(atlas_counts, merge_args(request, list(...)))
}

#' @export
#' @rdname atlas_counts
atlas_counts.default <- function(taxa = NULL, 
                                 filter = NULL, 
                                 geolocate = NULL,
                                 group_by = NULL, 
                                 limit = 100,
                                 type = c("record" ,"species"),
                                 refresh_cache = FALSE) {

  type <- match.arg(type)
  verbose <- getOption("galah_config")$verbose

  if(missing(group_by)) {
    query <- list()
    profile <- extract_profile(filter)
    query <- build_query(taxa, filter, geolocate, profile = profile)
    if (type == "species") {
      return(tibble(count = species_count(query)))
    }
    return(tibble(count = record_count(query)))
  }                  
  
  # if `groups` is as a vector
  if(!inherits(group_by, "galah_group_by")){
    group_by <- galah_group_by(group_by, expand = FALSE)
    if (getOption("galah_config")$run_checks) validate_fields(group_by$name)
  }
  
  # if all combinations of levels of `groups` are needed (expand = TRUE)
  if(attr(group_by, "expand") & nrow(group_by) > 1){ 
    
    # get counts given the filter provided by the user
    field_values_df <- atlas_counts_internal(
      taxa = taxa,
      filter = filter, 
      geolocate = geolocate,
      type = type,
      facets = group_by$name, 
      limit = NULL)
    n_fields_df <- data.frame(
      facets = group_by$name,
      n_fields = unlist(lapply(
        group_by$name, 
        function(a){length(which(!is.na(field_values_df[[a]])))})))

    # work out which to pass as facets vs those we iterate over with lapply
    facets_large <- n_fields_df$facets[which.max(n_fields_df$n_fields)]
    facets_small <- n_fields_df$facets[n_fields_df$facets != facets_large]

    # work out what combinations of `group`s should be sent to atlas_counts_internal
    levels_df <- expand.grid(
      lapply(
        field_values_df[, 
          which(names(field_values_df) %in% facets_small), 
          drop = FALSE], 
        function(a){a[!is.na(a)]}),
      stringsAsFactors = FALSE)
    levels_list <- split(levels_df, seq_len(nrow(levels_df)))
    filter_list <- lapply(levels_list, function(a){paste(colnames(a), a, sep = " == ")})
    
    # turn off validation, because 1. it's already done, and 2. it's slow
    initial_check_state <- getOption("galah_config")$run_checks
    if(initial_check_state){
      galah_config(run_checks = FALSE)
      on.exit(galah_config(run_checks = TRUE)) # correct placement?
    }
    
    # run `atlas_counts_internal` the requisite number of times
    if (verbose) { pb <- txtProgressBar(max = 1, style = 3) } # start progressbar
    
    result_list <- lapply(seq_along(levels_list),
      function(a){
        if (verbose) {
          val <- (a / length(levels_list))
          setTxtProgressBar(pb, val)
        }
        filter_this_loop <- galah_filter(filter_list[[a]])    
        filter_final <- rbind(filter, filter_this_loop)
        counts_query <- atlas_counts_internal(
          taxa = taxa,
          filter = filter_final,
          geolocate = geolocate,
          facets = n_fields_df$facets[which.max(n_fields_df$n_fields)],
          limit = limit,
          type = type)
        if(nrow(counts_query) > 0){   
          as.data.frame(list(levels_list[[a]], counts_query), row.names = NULL)
        }
      }) 
    if(verbose){close(pb)} # close progress bar
    return(as_tibble(do.call(rbind, result_list)))
     
  # if `groups` is of nrow == 1 (expand = FALSE)
  }else{
    atlas_counts_internal(
      taxa, filter, geolocate, 
      facets = group_by$name, 
      limit, type, refresh_cache,
      verbose = verbose)
  } 
}

# workhorse function to do most of the actual processing
## NOTE: need to turn off caching for multiple runs
atlas_counts_internal <- function(taxa = NULL, 
                                  filter = NULL, 
                                  geolocate = NULL,
                                  facets, # NOTE: not `groups` as no multiply section here
                                  limit = NULL, type = "record",
                                  refresh_cache = FALSE,
                                  verbose = FALSE # NOTE: internally `verbose` is manual, not from galah_config
                                  ) {
  
  page_size <- 100
  query <- list()
  profile <- extract_profile(filter)
  query <- build_query(taxa, filter, geolocate, profile = profile)
  
  # add facets in a way that supports single or multiple queries 
  facets_temp <- as.list(facets)
  names(facets_temp) <- rep("facets", length(facets_temp))
  query <- c(query, facets_temp)

  # build url etc
  url <- server_config("records_base_url")
  path <- "occurrence/facets"
  cache_file <- cache_filename("counts", unlist(query), limit, facets, type)

  caching <- getOption("galah_config")$caching
  if (caching && file.exists(cache_file) && !refresh_cache) {
    return(read_cache_file(cache_file))
  }

  total_cats <- total_categories(url, path, query)
  if (all(total_cats < 1)) {
    return(data.frame(name = as.character(), count = as.numeric()))
  }

  if (is.null(limit)) {
    limit <- sum(total_cats)
  }

  if (sum(total_cats) > limit && sum(total_cats) > page_size) {
    resp <- atlas_GET(url, path, params = query, paginate = TRUE, limit = limit,
                    page_size = page_size, offset_param = "foffset")
    counts <- data.table::rbindlist(lapply(resp, function(a) {
      data.frame(jsonlite::fromJSON(a)$fieldResult)
      }))
  } else {
      query$flimit <- max(limit)
      resp <- atlas_GET(url, path, params = query)
      counts <- data.table::rbindlist(resp$fieldResult)
  }

  if (sum(total_cats) > limit) {
    bullets <- c(
      glue::glue("This field has {total_cats} values. {limit} will be returned."),
      i = "Increase `limit` to return more values, or decrease `limit` to return fewer."
    )
    warn(bullets)
  }
  
  # parse out value
  value <- parse_fq(counts$fq)
  
  if (type == "species") {
    # this can take a while so add progress bar
    if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
    counts_final <- data.table::rbindlist(lapply(seq_along(value), function(x) {
      if (verbose) {
        val <- (x / length(value))
        setTxtProgressBar(pb, val)
      }
      species_query <- list()
      species_query$fq <- c(query$fq,
                            query_term(name = facets, value = value[[x]],
                            include = TRUE))
      count <- species_count(species_query)
      data.frame(name = value[[x]], count = count)
    }))
  } else {
    counts_final <- data.frame(
      name = value,
      count = counts$count)
  }
  
  if(length(facets) > 1){
    counts_final$field_name <- parse_field(counts$fq)
    counts_list <- split(counts_final, counts_final$field_name)
    counts_final <- as.data.frame(data.table::rbindlist(lapply(
      seq_along(facets), function(a){
        names(counts_list[[a]])[1] <- names(counts_list)[a]
        counts_list[[a]]
      }), 
      fill = TRUE))
     counts_final <- counts_final[, c(names(counts_list), "count")]
  }else{ # i.e. only one facet
    names(counts_final) <- c(facets, "count")
  }  
  
  attr(counts_final, "data_type") <- "counts"
  query <- data_request(taxa, filter, geolocate, groups = facets)
  attr(counts_final, "data_request") <- query
  
  if (caching) {
    write_cache_file(object = counts_final, data_type = "counts",
                     cache_file = cache_file)
  }
  
  return(counts_final |> as_tibble())
}

# get just the record count for a query
# handle too long queries in here?
record_count <- function(query) {
  query$pageSize <- 0
  url <- server_config("records_base_url")
  resp <- atlas_GET(url, "occurrences/search", query)
  resp$totalRecords
}

species_count <- function(query) {
  query$flimit <- 1
  query$facets <- "speciesID"
  url <- server_config("records_base_url")
  total_categories(url, "occurrence/facets", query)
}

validate_facet <- function(facet) {
  if (!all(facet %in% c(search_fields()$id, all_fields()$name))) {
    bullets <- c(
      glue::glue("\"{facet}\" is not a valid field."),
      i = "Use `show_all_fields()` to get a list of all valid options.",
      i = "Use `search_fields()` to search for the valid name of a specific field."
    )
    abort(bullets, call = caller_env())
  }
}

# Get number of categories of a filter
total_categories <- function(url, path, query) {
  query$flimit <- 1
  resp <- atlas_GET(url, path, params = query)
  if (is.null(resp$count)) {
    return(0)
  }
  resp$count
}

# # Extract filter names and values returned from API
parse_field <- function(fq){
  str_extract(fq, "^[:alnum:]+")
}

parse_fq <- function(fq){
  gsub("\"", "", sub("^[[:alnum:]]+:",  "", fq))
}
