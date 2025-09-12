#' Force evaluation of a database query
#' 
#' [coalesce()] is an S3 generic function intended to be called before 
#' [collapse()]. It is important as it shows the full set of queries 
#' required to properly evaluate the user's request. This is often broader 
#' than the single query returned by [collapse()]. If, for example,
#' the user's query includes a call to 
#' \code{\link[=identify.data_request]{identify()}}, then a taxonomic query
#' is required to run _before_ the 'final' query is attempted. In relation to
#' other functions that manipulate `_request` objects, [coalesce()] is called
#' within \code{\link[=collapse.data_request]{collapse()}}, and itself 
#' calls [as_query()] internally.
#' @rdname coalesce
#' @param x An object to be coalesced. Works for `data_request`, 
#' `metadata_request` and `file_request`.
#' @param ... Other arguments passed to [as_query()].
#' @order 1
#' @return An object of class `query_set`, which is simply a list of all `query` 
#' objects required to properly evaluate the specified request.
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [as_query()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
coalesce <- function(x, ...){
  UseMethod("coalesce")
}

#' @rdname coalesce
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @order 2
#' @export
coalesce.data_request <- function(x, mint_doi, ...){
  if(x$type == "distributions"){
    build_query_set_distributions(x)
  }else{
    build_query_set_data(x, mint_doi = mint_doi, ...)    
  }
}

#' @rdname coalesce
#' @order 3
#' @export
coalesce.metadata_request <- function(x, ...){
  if(potions::pour("package", "run_checks")){
    result <- switch(x$type, 
                     "fields-unnest" = list(request_metadata("fields") |> as_query()),
                     "profiles-unnest" = list(request_metadata("profiles") |> as_query()),
                     list())
  }else{
    result <- list()
  }
  if(grepl("-unnest$", x$type)){
    if(x$type == "taxa-unnest"){
      # identify() calls must be parsed, irrespective of `run_checks` (which is parsed above)
      if(!is.null(x$identify)){
        result[[(length(result) + 1)]] <- as_query_taxa(x) # best syntax for this??
      }
      if(is.null(x$identify) & is.null(x$filter)){
        cli::cli_abort("Requests of type `taxa-unnest` must also supply one of `filter()` or `identify()`.")
      }
    }else if(is.null(x$filter)){
      current_type <- x$type
      cli::cli_abort("Requests of type `{current_type}` containing `unnest` must supply `filter()`.")
    }
  }
  if(x$type == "lists-unnest"){
    query_obj <- as_query_lists_unnest(x, ...)
  }else{
    query_obj <- as_query(x)
  }
  result[[(length(result) + 1)]] <- query_obj
  class(result) <- "query_set"
  result
}

#' @rdname coalesce
#' @order 4
#' @export
coalesce.files_request <- function(x, 
                                   ...){
  # NOTE: switch is technically superfluous right now, but could be useful
  # for future file types
  result <- list(switch(x$type,
                        "media" = as_query_media_files(x, ...)
  ))
  class(result) <- "query_set"
  result
}

#' Internal function to build a `query_set` object 
#' for object of class `data_request`
#' @noRd
#' @keywords Internal
build_query_set_data <- function(x, mint_doi, ...){
  if(!missing(mint_doi)){
    x$mint_doi <- mint_doi
  }
  # x$type <- check_type(x$type) # needed?
  # handle sending dois via `filter()`
  # important this happens first, as it affects `type`, which affects later code
  variables <- x$filter$variable # NOTE: breaks for GBIF
  if(!is.null(variables)){
    if(length(variables) == 1 & variables[1] == "doi"){
      x$type <- "occurrences-doi"
    }
  }
  # handle `run_checks`
  fields_absent <- purrr::map(
    x[c("arrange", "filter", "select", "group_by")],
    is.null
  ) |>
    unlist()
  if (potions::pour("package", "run_checks") & 
      x$type != "occurrences-doi"){
    # add check here to see whether any filters are specified
    # it is possible to only call `identify()`, for example
    if (any(!fields_absent) | 
        x$type %in% c("species-count", "species")) {
      result <- list(request_metadata("fields") |> as_query(),
                     request_metadata("assertions") |> as_query())
    } else {
      # for living atlases, we need `collapse_fields()` to check the `lsid` field
      # this isn't required for GBIF which doesn't use `fq` for taxon queries
      if(!is.null(x$identify) &!is_gbif()){
        result <- list(request_metadata("fields") |> as_query())
      }else{
        result <- list()
      }
    }
    if (x$type %in% c("occurrences", "media", "species") &
        reasons_supported()) {
      result[[(length(result) + 1)]] <- request_metadata("reasons") |> 
        as_query()
    }
  } else { # if select is required, we need fields even if `run_checks == FALSE`
    if(!fields_absent[["select"]] | 
       x$type %in% c("occurrences", "species")){
      result <- list(request_metadata("fields") |> as_query(),
                     request_metadata("assertions") |> as_query())
    }else{
      result <- list()
    }
  }
  # handle `identify()`
  if(!is.null(x$identify) & 
     x$type != "occurrences-doi"){
    result[[(length(result) + 1)]] <- request_metadata() |>
      identify(x$identify) |>
      as_query()
  }
  # handle `apply_profile()`
  if(!is.null(x$data_profile)){
    result[[(length(result) + 1)]] <- request_metadata("profiles") |> 
      as_query()
  }
  # handle query
  result[[(length(result) + 1)]] <- as_query(x)
  class(result) <- "query_set"
  result
}

#' Internal function to build a `query_set` object 
#' for object of class `data_request` when `type = distributions`
#' @noRd
#' @keywords Internal
build_query_set_distributions <- function(x, ...){
  if(is.null(x$identify) & is.null(x$filter)){
    # find all expert distributions
    result <- list(
      as_query_distributions_metadata(),
      as_query_distributions_data(x)
    )
  }else{
    if(!is.null(x$identify)){
      result <- list(
        collapse_taxa(list(identify = x$identify)) # wrong syntax?
      )
      result[[2]] <- as_query_distributions_data(x) # NOTE: shouldn't call microfunctions directly
    }else{
      # i.e. !is.null(x$filter)
      result <- list(as_query_distributions_data(x))
    }
  }
  class(result) <- "query_set"
  result
}