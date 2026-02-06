#' Force evaluation of a database query
#' 
#' [compound()] is an S3 generic function intended to be called before 
#' [collapse()]. It is important as it shows the full set of queries 
#' required to properly evaluate the user's request. This is often broader 
#' than the single query returned by [collapse()]. If, for example,
#' the user's query includes a call to 
#' \code{\link[=identify.data_request]{identify()}}, then a taxonomic query
#' is required to run _before_ the 'final' query is attempted. In relation to
#' other functions that manipulate `_request` objects, [compound()] is called
#' within \code{\link[=collapse.data_request]{collapse()}}, and itself 
#' calls [capture()] internally where required.
#' @rdname compound
#' @param x An object to be compounded. Works for `data_request`, 
#' `metadata_request`, `file_request`, `query` or `prequery`.
#' @param ... Other arguments passed to [capture()].
#' @order 1
#' @return An object of class `query_set`, which is simply a list of all `query` 
#' objects required to properly evaluate the specified request. Objects are 
#' listed in the order in which they will be evaluated, meaning the query
#' that the user has actually requested will be placed last.
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [capture()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
compound <- function(x, ...){
  UseMethod("compound")
}

#' @rdname compound
#' @order 2
#' @export
compound.data_request <- function(x, mint_doi = FALSE, ...){
  x |>
    capture(mint_doi = mint_doi, ...) |>
    compound()
}

#' @rdname compound
#' @order 3
#' @export
compound.metadata_request <- function(x, ...){
  x |>
    capture(...) |>
    compound()
}

#' @rdname compound
#' @order 4
#' @export
compound.files_request <- function(x, 
                                   ...){
  x |>
    capture(...) |>
    compound()
}

#' @rdname compound
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"`, and only for supported atlases.
#' @order 5
#' @export
compound.prequery <- function(x, mint_doi = FALSE, ...){
  if(stringr::str_detect(x$type, "^metadata")){
    build_query_set_metadata(x)
  }else if(stringr::str_detect(x$type, "^files")){
    list(capture_media_files(x, ...)) |>
      as_query_set()
  }else if(x$type == "data/distribtions"){
    build_query_set_distributions(x)
  }else{
    build_query_set_data(x, mint_doi = mint_doi, ...)
  }
}

#' @rdname compound
#' @order 6
#' @export
compound.query <- function(x, ...){
  list(x) |>
    as_query_set()
}

#' @rdname compound
#' @order 7
#' @export
compound.query_set <- function(x, ...){
  x
}

#' Internal function to routinely apply `query_set` naming
#' @noRd
#' @keywords Internal
as_query_set <- function(x){
  structure(x, class = c("query_set", "list"))
}

#' Internal function to build a `query_set` object 
#' for object of class `data_request`
#' @noRd
#' @keywords Internal
build_query_set_metadata <- function(x){
  # create an empty object to store results
  result <- list()
  
  # handle authentication
  if(!is.null(x$request$authenticate)){
    result <- append(result,
                     list(request_metadata("config") |> capture()))
  }

  # add checks if required
  if(potions::pour("package", "run_checks")){
    result <- append(result,
                     switch(x$request$type, 
                            "fields-unnest" = list(request_metadata("fields") |> capture()),
                            "profiles-unnest" = list(request_metadata("profiles") |> capture()),
                            NULL))
  }
  
  # then handle `filter` and `identify` queries, where supported
  if(grepl("-unnest$", x$request$type)){
    if(x$request$type == "taxa-unnest"){
      # identify() calls must be parsed, irrespective of `run_checks` (which is parsed above)
      if(!is.null(x$request$identify)){
        result[[(length(result) + 1)]] <- list(type = "taxa",
             identify = x$request$identify) |>
          structure(class = "metadata_request") |>
          capture()
      }
      if(is.null(x$request$identify) & is.null(x$request$filter)){
        cli::cli_abort("Requests of type `taxa-unnest` must also supply one of `filter()` or `identify()`.")
      }
    }else if(is.null(x$request$filter)){
      current_type <- x$request$type
      cli::cli_abort("Requests of type `{current_type}` must supply `filter()`.")
    }
  }
  
  # add query in last place
  result[[(length(result) + 1)]] <- x
  
  # return object of correct class
  as_query_set(result)
}

#' Internal function to build a `query_set` object 
#' for object of class `data_request`
#' @noRd
#' @keywords Internal
build_query_set_data <- function(x, mint_doi, ...){
  
  # handle DOIs
  if(!missing(mint_doi)){
    x$request$mint_doi <- mint_doi
  }
  
  # set up an object
  result <- list()
  
  # handle authentication
  if(
    !is.null(x$request$authenticate) &
    potions::pour("atlas", "region") == "Australia"
  ){
    result <- append(result,
                     list(request_metadata("config") |> capture()))
  }
  
  # handle `run_checks`
  # find which functions are missing from the pipe
  lookup_fields <- c("arrange", "filter", "select", "group_by", "distinct")
  fields_absent <- !(lookup_fields %in% names(x$request))
  names(fields_absent) <- lookup_fields
  x_type <- x$request$type
  
  if(potions::pour("package", "run_checks") & 
      x_type != "occurrences-doi"){
    # add check here to see whether any filters are specified
    # it is possible to only call `identify()`, for example
    if(any(!fields_absent) | 
        x_type %in% c("species-count", "species")) {
      result <- c(result,
                  list(request_metadata("fields") |> 
                        select(tidyselect::everything()) |> # needed to ensure GBIF works
                        capture(),
                       request_metadata("assertions") |> capture()))
    }else{
      # for living atlases, we need `collapse_fields()` to check the `lsid` field
      # this isn't required for GBIF which doesn't use `fq` for taxon queries
      if(!is.null(x$request$identify) &
         !is_gbif()){
        result <- c(result,
                    list(request_metadata("fields") |> capture()))
      }
    }
    if(x_type %in% c("occurrences", "media", "species") &
       reasons_supported()) {
      result[[(length(result) + 1)]] <- request_metadata("reasons") |> 
        capture()
    }
  }else{ # if select is required, we need fields even if `run_checks == FALSE`
    if(!fields_absent[["select"]] | 
       x_type %in% c("occurrences", "species")){
      result <- c(result,
                  list(request_metadata("fields") |> capture(),
                       request_metadata("assertions") |> capture()))
    }
  }
  
  # handle `identify()`
  if(!is.null(x$request$identify) & 
     x_type != "occurrences-doi"){
    result[[(length(result) + 1)]] <- request_metadata() |>
      identify(x$request$identify) |>
      capture()
  }
  
  # handle `apply_profile()`
  if(!is.null(x$request$apply_profile)){
    result[[(length(result) + 1)]] <- request_metadata("profiles") |> 
      capture()
  }
  
  # handle query
  result[[(length(result) + 1)]] <- x
  
  # return
  as_query_set(result)
}

#' Internal function to build a `query_set` object 
#' for object of class `data_request` when `type = distributions`
#' @noRd
#' @keywords Internal
build_query_set_distributions <- function(x, ...){
  if(is.null(x$identify) & is.null(x$filter)){
    # find all expert distributions
    result <- list(
      capture_distributions_metadata(),
      capture_distributions_data(x)
    )
  }else{
    if(!is.null(x$identify)){
      result <- list(
        capture_taxa(list(identify = x$identify)) # wrong syntax?
      )
      result[[2]] <- capture_distributions_data(x) # NOTE: shouldn't call microfunctions directly
    }else{
      # i.e. !is.null(x$filter)
      result <- list(capture_distributions_data(x))
    }
  }
  as_query_set(result)
}