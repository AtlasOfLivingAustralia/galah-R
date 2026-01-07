#' Convert an object to class `query`
#'
#' Functionally similar to \code{\link[=collapse.data_request]{collapse()}}, but 
#' without passing through [coalesce()] first. Primarily an internal function, 
#' but exported for clarity and debugging purposes.
#' @details
#' Typically, queries in galah are piped using [galah_call()], which builds
#' an object of class `"data_request"`, `"metadata_request"` or 
#' `"files_request"`. All these objects can be converted to class `"query"` 
#' using \code{\link[=collapse.data_request]{collapse()}}. However,
#' \code{\link[=collapse.data_request]{collapse()}} first calls
#' \code{\link[=coalesce.data_request]{coalesce()}}, which expands to an
#' object of class `"query_set"` _before_ evaluating 
#' \code{\link[=collapse.data_request]{collapse()}}. In this context, 
#' [as_query()] serves two purposes: externally, it can be called to convert 
#' directly to class `"query"` without running checks; and internally it allows 
#' a query to be appended to a `"query_set"` without calling causing an 
#' infinite loop.
#' 
#' For simple cases, this gives the same result as running 
#' \code{\link[=collapse.data_request]{collapse()}} while the `run_checks` 
#' argument of [galah_config()] is set to `FALSE`, but is slightly faster. For 
#' complex cases, however, it is likely to generate irresolvable API calls, 
#' because e.g. taxonomic queries are not parsed before the URL is built. It 
#' should therefore be used with care.
#' @name as_query.data_request
#' @param x An object to convert to a `query`. Supported classes are the same
#' as those produced by [galah_call()], namely `data_request`, 
#' `metadata_request` or `files_request`.
#' @param ... Other arguments, currently ignored
#' @order 1
#' @return An object of class `query`, which is a list-like object containing 
#' two or more of the following slots:
#' 
#'  - `type`: The type of query, serves as a lookup to the corresponding field in `show_all(apis)`
#'  - `url`: Either:
#'    - a length-1 character giving the API to be queried; or 
#'    - a `tibble()` containing at least the field `url` and optionally others
#'  - `headers`: headers to be sent with the API call
#'  - `body`: body section of the API call
#'  - `options`: options section of the API call
#'  - `request`: captures the preceeding `_request` object (see [galah_call()])
#'
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [coalesce()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
as_query <- function(x, ...){
  UseMethod("as_query")
}

#' @rdname as_query.data_request
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @order 2
#' @export
as_query.data_request <- function(x,
                                  mint_doi = FALSE,
                                  ...){
  x <- x |> 
    check_authentication() |>
    check_distinct_count_groupby() |>
    check_slice_arrange() |>
    enforce_select_query()
  switch(x$type,
         "occurrences" = as_query_occurrences(x, mint_doi = mint_doi),
         "occurrences-count" = as_query_occurrences_count(x),
         "occurrences-doi" = as_query_occurrences_doi(x),
         "species" = as_query_species(x),
         "species-count" = as_query_species_count(x),
         "distributions" = as_query_distributions_data(x),
         cli::cli_abort("Unrecognised 'type'")) |>
  add_request(x)
}

#' @rdname as_query.data_request
#' @order 3
#' @export
as_query.metadata_request <- function(x, ...){
  x <- x |>
    check_authentication() |>
    enforce_select_query()
  switch(x$type,
         "apis" = as_query_apis(x),
         "assertions" = as_query_assertions(x),
         "atlases" = as_query_atlases(x),
         "collections" = as_query_collections(x),
         "config" = as_query_config(x),
         "datasets" = as_query_datasets(x),
         "distributions" = as_query_distributions_metadata(x),
         "fields" = as_query_fields(x),
         "fields-unnest" = as_query_fields_unnest(x),
         "licences" = as_query_licences(x),
         "lists" = as_query_lists(x),
         "lists-unnest" = as_query_lists_unnest(x),
         "media" = as_query_media_metadata(x),
         "profiles" = as_query_profiles(x),
         "profiles-unnest" = as_query_profiles_unnest(x),
         "providers" = as_query_providers(x),
         "ranks" = as_query_ranks(x),
         "reasons" = as_query_reasons(x),
         "taxa" = as_query_taxa(x),
         "taxa-unnest" = as_query_taxa_unnest(x),
         "identifiers" = as_query_identifiers(x),
         cli::cli_abort("Unrecognised 'type'")
         ) |>
  add_request(x)
}

#' @rdname as_query.data_request
#' @param thumbnail Logical: should thumbnail-size images be returned? Defaults 
#' to `FALSE`, indicating full-size images are required.
#' @order 4
#' @export
as_query.files_request <- function(x, 
                                   thumbnail = FALSE,
                                   ...){
  # NOTE: switch is technically superfluous right now, but could be useful
  # for future file types
  
  # This code is identical to `collapse.files_request()`
  switch(x$type,
         "media" = as_query_media_files(x, 
                                        thumbnail = thumbnail),
         cli::cli_abort("Unrecognised 'type'")) |>
  add_request(x)
}

#' @rdname as_query.data_request
#' @order 5
as_query.list <- function(x){
  # TODO add some checks here?
  structure(x, class = c("query", "list"))
}

#' @rdname as_query.data_request
#' @order 6
as_query.query <- function(x){
  x
}

#' Internal function called by `as_query()`
#' @noRd
#' @keywords Internal
count_switch <- function(x){ 
  x$type <- switch(x$type, 
                   "occurrences" = "occurrences-count",
                   "occurrences-count" = "occurrences-count",
                   "species" = "species-count",
                   "species-count" = "species-count",
                   "media" = cli::cli_abort("type = 'media' is not supported by `count()`"),
                   cli::cli_abort("`count()` only supports `type = 'occurrences' or` `'species'`"))
  x
}

#' Internal function to check behaviour of `distinct()`, `group_by()` etc.
#' called by `as_query()`
#' @noRd
#' @keywords Internal
check_distinct_count_groupby <- function(x){

  # get basic info
  has_group_by <- !is.null(x$group_by)
  has_count <- !is.null(x$count)
  has_distinct <- !is.null(x$distinct)
  has_select <- !is.null(x$select)

  # if type is 'species', ensure `distinct` is added
  # this is clunky, but backwards compatible
  if(x$type == "species" & !has_distinct){
    if(has_count){
      x <- x |> distinct("speciesID", .keep_all = FALSE)
    }else{
      x <- x |> distinct("speciesID", .keep_all = TRUE)
    }
    has_distinct <- TRUE
  }

  # first handle case when distinct() is supplied
  if(has_distinct){
    has_distinct_name <- !is.na(x$distinct$name)
    keep_all <- x$distinct$keep_all
    if(has_distinct_name){
      if(keep_all){ # this section feels incomplete
        update_request_object(x, type = "species")
      }else{ # keep_all is FALSE
        update_request_object(x, type = "species-count")
      }
    }else{ # no distinct name
      if(has_group_by){
        if(keep_all){
          x <- update_request_object(x, type = "species")
          if(has_count){
            count_switch(x)
          }else{
            x
          }
        }else{
          x <- update_request_object(x, type = "occurrences-count")
          if(has_select){
            if(has_count){
              x
            }else{
              dplyr::select(x, -dplyr::any_of("count"))
            } # end has_count
          }else{ # end has_select
            if(has_count){
              dplyr::select(x, -dplyr::any_of(c("label", "i18nCode", "fq")))
            }else{
              dplyr::select(x, -dplyr::any_of(c("label", "i18nCode", "fq", "count")))
            }
          } # end has_select
        } # end keep_all = FALSE
      }else{ # no group_by() AND empty call distinct()
        if(has_count){
          count_switch(x)
        }else{
          x
        }
      } # end has_group_by
    } # end has_distinct_name
  }else{ # no distinct() call
    if(has_select){
      if(has_count){
        count_switch(x)
      }else{
        x
      }
    }else{ # no select
      if(has_count){
        x |>
          count_switch() |>
          dplyr::select(-dplyr::any_of(c("label", "i18nCode", "fq")))
      }else{
        dplyr::select(x, group = "basic") # assumes type = "occurrences"
      }
    } # end has_select
  } # end has_distinct
} # end function
  
#' Internal function to check `slice` and `arrange` for counts
#' @keywords Internal
#' @noRd
check_slice_arrange <- function(x){
  if(!stringr::str_detect(x$type, "-count$")){
    x
  }else{
    if(is.null(x$slice)){
      # limits to 10,000 rows
      # TODO: This should ultimately be set by `slice` or `atlas_counts(limit = )`, not internally.
      #       Will need updating to avoid hidden limit setting here & in `compute_occurrences_count()`
      slice <- tibble::tibble(slice_n = 1e4, slice_called = FALSE) 
    }else{
      slice <- x$slice
    }
    if(is.null(x$arrange)){
      arrange <- tibble::tibble(variable = "count", 
                                direction = "descending")
    }else{
      arrange <- x$arrange
    }
    x$slice_arrange <- dplyr::bind_cols(slice, arrange)
    x$arrange <- NULL
    x$slice <- NULL
    x
  }
}

#' Internal function to enforce `select()` for metadata queries. Basically just 
#' supplies defaults. This is the *setup* phase as is usually called by 
#' `as_query()`
#' @noRd
#' @keywords Internal
enforce_select_query <- function(x){
  # note: UseMethod() would be tidier, but seems to need to be exported to work?
  switch(class(x)[1],
    "metadata_request" = enforce_select_query_metadata(x),
    "data_request" = enforce_select_query_data(x),
    x)
}

#' sub-function to `enforce_select_query()`
#' @noRd
#' @keywords Internal
enforce_select_query_metadata <- function(x){
  # if `select()` is given, we simply pass it on
  # if missing, we have to apply some logic
  if(is.null(x$select)){
    specific_type <- x |>
      purrr::pluck("type") |>
      stringr::str_remove("^metadata/")
    # see whether `lookup_select_columns()` returns anything
    chosen_columns <- lookup_select_columns(specific_type)  
    # some `unnest` queries internally rename the lead column to the name of the supplied field
    if(is.null(chosen_columns) & 
       stringr::str_detect(specific_type, "-unnest$")){
         chosen_columns <- x$filter |>
           purrr::pluck("value")
    }
    # if we have, after 2 attempts, found some chosen_columns, use them
    if(!is.null(chosen_columns)){
      x <- dplyr::select(x, tidyselect::any_of({{chosen_columns}}))
    # if *still* null, choose `everything()`
    }else{
      x <- dplyr::select(x, tidyselect::everything()) 
    }
  }else{
    x
  }
}

#' sub-function to `enforce_select_query()`
#' @noRd
#' @keywords Internal
enforce_select_query_data <- function(x){
  if(is.null(x$select)){
    switch(x$type, 
      "occurrences" = dplyr::select(x, group = "basic"),
      "occurrences-count" = dplyr::select(x, -dplyr::any_of(c("label", "i18nCode", "fq"))),
      "species" = dplyr::select(x, group = "taxonomy"),
      # NOTE: further exceptions may be needed for type = "species"
      dplyr::select(x, tidyselect::everything()) # useful for type = "occurrences-doi"
    )    
  }else{
    x
  }
}

#' @noRd
#' @keywords Internal
add_request <- function(new_obj, source_obj){
  new_class <- class(new_obj)
  new_obj$request <- source_obj
  structure(new_obj, class = new_class)
}