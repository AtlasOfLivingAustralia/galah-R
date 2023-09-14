#' Internal function to check whether first object is of class `data_request`
#' Called exclusively by `atlas_` functions
#' @noRd
#' @keywords Internal
check_atlas_inputs <- function(args){
  if(!is.null(args$request)){
    check_data_request(args$request)
    result <- update_data_request(args$request, args[-1])
  }else{
    result <- do.call(galah_call, args)
  }
  return(result)
}

#' function to check whether `type` arg is supplied correctly to `collapse()` or
#' `compute()`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_type <- function(type){
  if(type == "records"){
    type <- "occurrences-count"
  }
  valid_types <- c(
    "occurrences", "occurrences-count",
    "doi", 
    "media",
    "taxonomy",
    "species",  "species-count")
  if(!(type %in% valid_types)){
    abort("`type` not recognised")
  }
  type
}

# check_media_types <- function(.data, media){
#   if(is.null(.data$media))
# }

#' Internal function to convert multi-value media fields to list-columns
#' @param .data A tibble() returned by atlas_occurrences
#' @noRd
#' @keywords Internal
check_media_cols <- function(.data){
  media_colnames <- c("images", "sounds", "videos")
  # if media columns are not present, return original data unchanged
  if(!any(colnames(.data) %in% media_colnames)){
    .data
  }
  # otherwise get media columns
  present_cols <- media_colnames[media_colnames %in% colnames(.data)]
  for(i in present_cols){
    if(!all(is.na(.data[[i]]))){
      .data[[i]] <- strsplit(.data[[i]], "\\s\\|\\s")
    }
  }
  .data
}

#' Internal function to confirm requisite login information has been provided
#' Called by `compute()`
#' @noRd
#' @keywords Internal
#' @importFrom rlang caller_env
check_login <- function(.data, error_call = caller_env()) {
  # Check for valid email for occurrences or species queries for all providers
  if(.data$type == "occurrences" | .data$type == "species"){
    switch(pour("atlas", "region"), 
           "United Kingdom" = return(),
           "Global" = {check_email(.data)
                       check_password(.data)},
           check_email(.data))
  }
  # ALA requires an API key
  # } else if (pour("atlas", "acronym") == "ALA") {
  #   if (is.null(.data$headers$`x-api-key`) | .data$headers$`x-api-key` == "") {
  #     abort_api_key_missing()
  #   }
  # }  
}

#' Subfunction to `check_login()`
#' @noRd
#' @keywords Internal
check_email <- function(.data){
  email_text <- url_parse(.data$url)$query$email
  if(is.null(email_text)) {
    abort_email_missing()
  }else if(email_text == ""){
    abort_email_missing()
  }
}

#' Subfunction to `check_login()`
#' @noRd
#' @keywords Internal
check_password <- function(.data){
  if (.data$opts$userpwd == ":") {
    abort("GBIF requires a username and password to download occurrences or species",
          call = error_call
    )
  }
}

#' Internal function to check that the specified path exists, and if not,
#' to create it. Called by `galah_config()`
#' @param x a path to a directory, or NULL
#' @noRd
#' @keywords Internal
check_directory <- function(x){
  if(is.null(x)){
    cache_dir <- tempfile()
    dir.create(cache_dir)
    return(cache_dir)
  }else{
    if(!dir.exists(dirname(x))){
      bullets <- c(
        "Cannot find directory.",
        i = "Please enter a valid directory and try again.",
        x = glue("{x} does not exist.")
      )
      abort(bullets, call = error_call)
    }else{
      x
    }
  }
}

# Internal function to create a valid filename for download
# Note this is most commonly used when galah defaults are in place; i.e. 
# downloads are sent to a temporary directory.
# Called by `query_API()`
# check_path <- function(.data){
#   if(is.null(.data$path)){
#     if(.data$type == "species"){
#       ext <- "csv"
#     }else{
#       ext <- "zip"
#     }
#     cache_file <- pour("package", "directory")
#     .data$path <- paste0(cache_dir, "/temp_file.", ext)    
#   } else {
#     dirname(x) |> check_directory() # errors if path doesn't exist
#     # NOTE: it might make sense here to check that a supplied filename is valid
#   }
#   .data
# }

#' Internal function to expand a url
#' 
#' Proposed function to spin out multiple urls to paginate when n is high
#'  
#' Note: this needs to be in the compute stage of multiple APIs: ie. from `request_data()` and `request_metadata()`
#' Also requires something like `check_facet_count()` to know what the max value is.
#' @noRd
#' @keywords Internal
# check_pagination <- function(){}

#' Check whether geolocate functions have >1 argument
#' @noRd
#' @keywords Internal
check_n_inputs <- function(dots, error_call = caller_env()) {
  if(length(dots) > 1){
    n_geolocations <- length(dots)
    bullets <- c(
      "More than 1 spatial area provided.",
      "*" = glue("Using first location, ignoring additional {n_geolocations - 1} location(s).")
    )
    warn(bullets, call = caller_env())
  }
}

#' Internal function to check taxonomic argument provided to `atlas_` functions is of correct form
#' @noRd
#' @keywords Internal
check_taxa_arg <- function(taxa, error_call = caller_env()) {
  # if (!any(grepl("id", class(taxa)))) {
  if(!inherits(taxa, "tibble")){
    if (colnames(taxa)[1] != "search_term") {
      bullets <- c(
        "Wrong type of input provided to `identify` argument.",
        i = glue("`identify` requires an identifier input generated by \\
        `galah_identify`.")
      )
      abort(bullets, call = error_call)
    }
  }
}

check_data_request <- function(request, error_call = caller_env()){
  if(!inherits(request, "data_request")){
    bullets <- c(
      "Argument `request` requires an object of type `data_request`.",
      i = "You can create this object using `galah_call()`",
      i = "Did you specify the incorrect argument?"
    )
    abort(bullets, call = caller_env())      
  }     
}

# Check for valid `type`
check_type_valid <- function(type, valid, error_call = caller_env()) {
  if(!any(valid == type)){
    bullets <- c(
      glue("type `{type}` is not recognised"),
      i = "see ?show_all for a list of valid information types."
    )
    abort(bullets, call = error_call)   
  }
}

check_named_input <- function(dots){
  name_length <- any(length(names(dots) > 0)) & any(names(dots) != "")
  if(name_length){
    bullets <- c(
      "We detected a named input.",
      i = "This usually means that you've used `=` instead of `==`.")
    abort(bullets)
  }
}

# If no args are supplied, set default columns returned as group = "basic"
check_groups <- function(group, n){
  if(missing(group)){
    if(n < 1){
      "basic"
    }else{
      NULL
    }
  }else{
    match.arg(group, 
              choices = c("basic", "event", "media", "assertions"),
              several.ok = TRUE)
  }
}

#' Internal function to check whether fields are valid
#' @importFrom dplyr pull
#' @importFrom glue glue_data
#' @importFrom httr2 url_parse
#' @importFrom rlang format_error_bullets
#' @importFrom tidyr drop_na
#' @noRd
#' @keywords Internal
check_fields <- function(.data) {
    
  # other behaviour depends on galah_config() settings
  if (pour("package", "run_checks")) {
    queries <- url_parse(.data$url[1])$query

    # extract fields from filter & identify
    filter_invalid <- NA
    if(is.null(queries$fq)){
      # note: above was previously: `exists("fq", where = queries)`
      # Error in as.environment(where) : using 'as.environment(NULL)' is defunct
      filters <- NULL
    }else{
      if (nchar(queries$fq) > 0) {
        filters <- string_to_tibble(queries$fq) |>
          pull(value) |>
          gsub("\\(|\\)|\\-|\\:", "", x = _)
      } else {
        filters <- NULL
      }
      
      if (length(filters) > 0) {
        if (!all(filters %in% show_all_fields()$id)) {
          invalid_fields <- filters[!(filters %in% c(show_all_fields()$id, show_all_assertions()$id))]
          filter_invalid <- glue::glue_collapse(invalid_fields,
                                                sep = ", ")
        }
      }
    }
    
    # galah_select columns check
    select_invalid <- NA
    if (!is.null(queries$fields)) {
      fields <- queries$fields |>
        strsplit(",") |>
        unlist()
      if (length(fields) > 0) {
        if (!all(fields %in% show_all_fields()$id)) {
          invalid_fields <- fields[!(fields %in% c(show_all_fields()$id, show_all_assertions()$id))]
          list_invalid_fields <- glue::glue_collapse(invalid_fields,
                                                     sep = ", ")
          select_invalid <- glue::glue_collapse(invalid_fields,
                                                     sep = ", ")
        }
      }
    }
    
    # galah_group_by fields check
    group_by_invalid <- NA
    if (!is.null(queries$facets)) {
      facets <- queries[names(queries) == "facets"] |> unlist() # NOTE: arrange() is missing
      if (length(facets) > 0) {
        if (!all(facets %in% show_all_fields()$id)) {
          invalid_fields <- facets[!(facets %in% c(show_all_fields()$id, show_all_assertions()$id))]
          group_by_invalid <- glue::glue_collapse(invalid_fields,
                                                     sep = ", ")
        }
      }
    }
    
    # error message
    if(any(!is.na(c(filter_invalid, select_invalid, group_by_invalid)))) {
      returned_invalid <- tibble(
        function_name = c("`galah_filter`", "`galah_select`", "`galah_group_by`"),
        fields = c(filter_invalid, select_invalid, group_by_invalid)
        ) |>
        tidyr::drop_na()
      
      glue_template <- " {returned_invalid$function_name}: {returned_invalid$fields}"
      invalid_fields_message <- glue::glue_data(returned_invalid, glue_template, .na = "")
      
      bullets <- c(
        "Can't use fields that don't exist.",
        # i = "Use `show_all(fields)` to see all valid fields.",
        i = "Use `search_all(fields)` to find a valid field ID.",
        x = glue("Unrecognised field(s):"),
        rlang::format_error_bullets(invalid_fields_message)
      )
      abort(bullets)
    }
  } # end if(run_checks)
}

#' function to replace search terms with identifiers via `search_taxa()`  
#' @importFrom dplyr pull
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @importFrom purrr pluck
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @noRd
#' @keywords Internal
check_lazy_identifiers <- function(.data){
  url <- url_parse(.data$url[1]) # FIXME: test if every >1 urls here
  queries <- url$query
  if(!is.null(queries$fq)){
    taxa_search_terms <- str_extract_all(queries$fq, "`[:alpha:]+`") |> # FIXME: need a way to not add `` if search = FALSE
      pluck(!!!list(1)) 
    if(length(taxa_search_terms) > 0){
      taxa_ids <- taxa_search_terms |>
        str_remove_all("`") |>
        parse_identify(search = TRUE) |> # note: this is still in `galah_identify.R`
        pull(identifier)
      queries$fq <- str_replace_all(queries$fq, taxa_search_terms, taxa_ids)
      url$query <- queries
      .data$url[1] <- url_build(url)
    }
  }
  .data
}

# check that objects passed within `galah_filter` have correct structure
check_filter_tibbles <- function(x){ # where x is a list of tibbles
  syntax_valid <- lapply(x, function(a){
    if(length(colnames(a)) == 4){
      all(colnames(a) %in% c("variable", "logical", "value", "query"))
    }else{
      FALSE
    }
  }) |>
    unlist() |>
    all()
  if(!syntax_valid){
    abort("There was a problem with `filter`, did you use correct syntax?")
  }
}