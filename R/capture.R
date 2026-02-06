#' Capture a request
#'
#' @description
#' The first step in evaluating a request is to capture and parse the 
#' information it contains. The resulting object has class `prequery`
#' for those requiring further processing or `query` for those that don't. 
#' A `prequery` object shows what has been requested by a user in a given 
#' `galah_call()`.
#' 
#' @details
#' Typically, queries in galah are piped using [galah_call()], which builds
#' an object of class `"data_request"`; or [request_metadata()] or
#' [request_files()]. Under the hood, [galah_call()] constructs a query 
#' ([collapse()]), sends it to an API ([compute()]), and returns its 
#' contents ([collect()]).
#' 
#' For [collapse()] to construct a complete query (class `"query"`), 
#' it often requires additional, smaller API calls to validate pieces 
#' of information used to construct the full user request. 
#' Underlying [collapse()] are these 
#' smaller but necessary steps:
#'   * [capture()] parses the basic structure of a user request
#'   * [compound()] combines all necessary API calls to construct the final query
#'   * [collapse()] constructs the complete query
#'
#' A `prequery` object shows what has been requested, before those 
#' calls are built by [compound()] and evaluated by  
#' \code{\link[=collapse.data_request]{collapse()}}.
#' For simple cases, this gives the same result as running 
#' \code{\link[=collapse.data_request]{collapse()}} while the `run_checks` 
#' argument of [galah_config()] is set to `FALSE`, but is slightly faster.
#' In complex cases, it is simply a precursor to [compound()]
#' @name capture.data_request
#' @param x A `_request` object to convert to a `prequery`.
#' @param ... Other arguments, currently ignored
#' @order 1
#' @return Either an object of class `prequery` when further processing is 
#' required; or `query` when it is not. Both classes are structurally identical,
#' being list-like and containing at the following slots:
#' 
#'  - `type`: The type of query, serves as a lookup to the corresponding field in `show_all(apis)`
#'  - `url`: Either:
#'    - a length-1 character giving the API to be queried; or 
#'    - a `tibble()` containing at least the field `url` and optionally others
#'  - `request`: captures the preceeding `_request` object (see [galah_call()])
#'
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [compound()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
capture <- function(x, ...){
  UseMethod("capture")
}

#' @rdname capture.data_request
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @order 2
#' @export
capture.data_request <- function(x,
                                 mint_doi = FALSE,
                                 ...){
  x <- x |> 
    check_authentication() |>
    check_doi() |>
    check_distinct_count_groupby() |>
    check_glimpse() |>
    check_slice_arrange() |>
    enforce_select_query()
  switch(x$type,
         "occurrences" = capture_occurrences(x, mint_doi = mint_doi),
         "occurrences-count" = capture_occurrences_count(x),
         "occurrences-doi" = capture_occurrences_doi(x),
         "occurrences-glimpse" = capture_occurrences_glimpse(x),
         "species" = capture_species(x),
         "species-count" = capture_species_count(x),
         "distributions" = capture_distributions_data(x),
         cli::cli_abort("Unrecognised 'type'")) |>
  add_request(x)
}

#' @rdname capture.data_request
#' @order 3
#' @export
capture.metadata_request <- function(x, ...){
  x <- x |>
    check_authentication() |>
    enforce_select_query()
  switch(x$type,
         "apis" = capture_apis(x),
         "assertions" = capture_assertions(x),
         "atlases" = capture_atlases(x),
         "collections" = capture_collections(x),
         "config" = capture_config(x),
         "datasets" = capture_datasets(x),
         "distributions" = capture_distributions_metadata(x),
         "fields" = capture_fields(x),
         "fields-unnest" = capture_fields_unnest(x),
         "licences" = capture_licences(x),
         "lists" = capture_lists(x),
         "lists-unnest" = capture_lists_unnest(x),
         "media" = capture_media_metadata(x),
         "profiles" = capture_profiles(x),
         "profiles-unnest" = capture_profiles_unnest(x),
         "providers" = capture_providers(x),
         "ranks" = capture_ranks(x),
         "reasons" = capture_reasons(x),
         "taxa" = capture_taxa(x),
         "taxa-unnest" = capture_taxa_unnest(x),
         "identifiers" = capture_identifiers(x),
         cli::cli_abort("Unrecognised 'type'")
         ) |>
  add_request(x)
  # FIXME: If authentication is added, this should change from being a `query` to a `prequery`
}

#' @rdname capture.data_request
#' @param thumbnail Logical: should thumbnail-size images be returned? Defaults 
#' to `FALSE`, indicating full-size images are required.
#' @order 4
#' @export
capture.files_request <- function(x, 
                                   thumbnail = FALSE,
                                   ...){
  # NOTE: switch is technically superfluous right now, but could be useful
  # for future file types
  
  # This code is identical to `collapse.files_request()`
  switch(x$type,
         "media" = capture_media_files(x, 
                                        thumbnail = thumbnail),
         cli::cli_abort("Unrecognised 'type'")) |>
  add_request(x)
}

#' @rdname capture.data_request
#' @order 5
capture.list <- function(x){
  as_prequery(x)
}

#' Internal function to enforce class `query`
#' @noRd
#' @keywords Internal
as_query <- function(x){
  structure(x, class = c("query", "list"))
}

#' Internal function to enforce class `prequery`
#' @noRd
#' @keywords Internal
as_prequery <- function(x){
  structure(x, class = c("prequery", "list"))
}

#' Internal function to ensure that DOIs are parsed properly
#' @noRd
#' @keywords Internal
check_doi <- function(x){
  if(x$type == "occurrences"){
    # handle sending dois via `filter()`
    # important this happens first, as it affects `type`, which affects later code
    variables <- purrr::pluck(x, "filter", "variable") # NOTE: breaks for GBIF
    if(!is.null(variables)){
      if(length(variables) == 1 & variables[1] == "doi"){
        x$type <- "occurrences-doi"
      }
    }
  }
  x
}

#' Internal function to check behaviour of `distinct()`, `group_by()` etc.
#' called by `capture()`
#' @noRd
#' @keywords Internal
check_distinct_count_groupby <- function(x){

  if(x$type == "occurrences-doi"){
    return(x)
  }

  # get basic info
  has_group_by <- !is.null(x$group_by)
  has_count <- !is.null(x$count)
  has_distinct <- !is.null(x$distinct)
  has_select <- !is.null(x$select)

  # if type is 'species', ensure `distinct` is added
  # this is clunky, but backwards compatible
  if(x$type == "species" & !has_distinct){
    if(has_count){
      x <- x |> distinct(species_facets(), .keep_all = FALSE)
    }else{
      x <- x |> distinct(species_facets(), .keep_all = TRUE)
    }
    has_distinct <- TRUE
  }

  # first handle case when distinct() is supplied
  if(has_distinct){
    has_distinct_name <- !is.na(x$distinct$name)
    keep_all <- x$distinct$keep_all
    if(has_distinct_name){
      if(keep_all){
        update_request_object(x, type = "species")
      }else{ # keep_all is FALSE
        if(has_group_by){
          x |>
            update_request_object(type = "species-count")
        }else{
          if(has_count){
            # counts the number of facets
            x |>
              update_request_object(type = "species-count")
          }else{
            # designed to be equivalent to `show_values()`
            x |>
              update_request_object(type = "occurrences-count") |>
              dplyr::select(-dplyr::any_of(c("label", "i18nCode", "fq", "count")))
          }
        }
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
  
#' Internal function called by `capture()`
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

#' Internal function to capture `glimpse()` calls
#' @noRd
#' @keywords Internal
check_glimpse <- function(x){
  if(!is.null(x$glimpse)){
    if(x$type == "occurrences"){
      x$type <- "occurrences-glimpse"
    }else{
      cli::cli_inform("`glimpse()` is only supported for `type =\"occurrences\"")
    }
  }
  x
}
  
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
#' `capture()`
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