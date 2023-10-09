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

#' Internal function to check for `data_request`s
#' @noRd
#' @keywords Internal
check_data_request <- function(request, error_call = caller_env()){
  if(!inherits(request, "data_request")){
    bullets <- c(
      "Argument `.data` requires an object of type `data_request`.",
      i = "You can create this object using `galah_call()`",
      i = "Did you specify the incorrect argument?"
    )
    abort(bullets, call = caller_env())      
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

#' Internal function to ensure a download file is given
#' @noRd
#' @keywords Internal
check_download_filename <- function(file, ext = "zip"){
  if(!is.null(file)){
    file
  }else{
    cache_directory <- pour("package", "directory", .pkg = "galah")
    current_time <- Sys.time() |> format("%Y-%m-%d_%H-%M-%S")
    file <- glue("{cache_directory}/data_{current_time}.{ext}") |>
      as.character()
    # check_path()? # currently commented out in check.R
  }
  return(file)
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

#' Internal function to confirm requisite login information has been provided
#' Called by `compute()`
#' @noRd
#' @keywords Internal
#' @importFrom rlang caller_env
check_login <- function(.data, error_call = caller_env()) {
  # Check for valid email for occurrences or species queries for all providers
  if(.data$type == "data/occurrences" | .data$type == "data/species"){
    switch(pour("atlas", "region"), 
           "United Kingdom" = return(),
           "Global" = {
             check_email(.data)
             check_password(.data)},
           check_email(.data))
  }
  .data
  # ALA requires an API key
  # } else if (pour("atlas", "acronym") == "ALA") {
  #   if (is.null(.data$headers$`x-api-key`) | .data$headers$`x-api-key` == "") {
  #     abort_api_key_missing()
  #   }
  # }  
}

#' Internal function to check whether fields are valid
#' @importFrom dplyr bind_rows
#' @importFrom dplyr pull
#' @importFrom glue glue_collapse
#' @importFrom glue glue_data
#' @importFrom httr2 url_parse
#' @importFrom rlang format_error_bullets
#' @importFrom tidyr drop_na
#' @noRd
#' @keywords Internal
check_fields <- function(.data) {
  
  # other behaviour depends on galah_config() settings
  if (pour("package", "run_checks")) {
    url <- url_parse(.data$url[1])
    queries <- url$query

    # set fields to check against
    # NOTE: These are retrieved in collapse()
    valid_fields <- .data[["metadata/fields"]]$id
    valid_assertions <- .data[["metadata/assertions"]]$id
    valid_any <- c(valid_fields, valid_assertions)
    
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
        if (!all(filters %in% valid_any)) {
          invalid_fields <- filters[!(filters %in% valid_any)]
          filter_invalid <- glue_collapse(invalid_fields, sep = ", ")
        }
      }
    }
    
    # galah_select columns check - note distinction between fields and assertions
    select_invalid <- NA
    if (!is.null(queries$fields)) {
      fields <- queries$fields |>
        strsplit(",") |>
        unlist()
      if (length(fields) > 0) {
        assertions_check <- fields %in% valid_assertions
        if(any(assertions_check)){
          if(queries$qa == "none"){
            queries$qa <- glue_collapse(fields[assertions_check], sep = ",")
            queries$fields <- glue_collapse(fields[!assertions_check], sep = ",")
            url$query <- queries
            .data$url[1] <- url_build(url)
          } # no else{}, as only other possible option is "all"
          fields <- fields[!assertions_check]
        }
        if (!all(fields %in% valid_fields)) {
          invalid_fields <- fields[!(fields %in% valid_fields)]
          list_invalid_fields <- glue_collapse(invalid_fields, sep = ", ")
          select_invalid <- glue_collapse(invalid_fields, sep = ", ")
        }
      }
    }
    
    # galah_group_by fields check
    group_by_invalid <- NA
    if (!is.null(queries$facets)) {
      facets <- queries[names(queries) == "facets"] |> unlist() # NOTE: arrange() is missing
      if (length(facets) > 0) {
        if (!all(facets %in% valid_any)) {
          invalid_fields <- facets[!(facets %in% valid_any)]
          group_by_invalid <- glue_collapse(invalid_fields, sep = ", ")
        }
      }
    }
    
    # error message
    if(any(!is.na(c(filter_invalid, select_invalid, group_by_invalid)))) {
      returned_invalid <- tibble(
        function_name = c("`filter`", "`select`", "`group_by`"),
        fields = c(filter_invalid, select_invalid, group_by_invalid)
      ) |>
        drop_na()
      
      glue_template <- " {returned_invalid$function_name}: {returned_invalid$fields}"
      invalid_fields_message <- glue_data(returned_invalid, glue_template, .na = "")
      
      bullets <- c(
        "Can't use fields that don't exist.",
        # i = "Use `show_all(fields)` to see all valid fields.",
        i = "Use `search_all(fields)` to find a valid field ID.",
        x = glue("Unrecognised field(s):"),
        format_error_bullets(invalid_fields_message)
      )
      abort(bullets)
    }
  }
  .data
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
check_identifiers <- function(.data){
  url <- url_parse(.data$url[1]) # FIXME: test if every >1 urls here
  queries <- url$query
  if(!is.null(queries$fq)){
    if(grepl("(`TAXON_PLACEHOLDER`)", queries$fq)){
      metadata_lookup <- grepl("^metadata/taxa", names(.data))
      if(any(metadata_lookup)){
        identifiers <- .data[[which(metadata_lookup)[1]]]
        taxa_ids <- build_taxa_query(identifiers$taxon_concept_id)
        queries$fq <- str_replace_all(queries$fq, 
                                      "\\(`TAXON_PLACEHOLDER`\\)", 
                                      taxa_ids)
        url$query <- queries
        .data$url[1] <- url_build(url)
      }else{
        # this only happens if there is a bug earlier in the code
        abort("The query has a taxonomic placeholder, but taxon search has been run.")
      }
    }
  }
  .data
}

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

#' Internal function to check whether valid media fields have been supplied
#' @param .data a `query` object
#' @noRd
#' @keywords Internal
check_media_cols_present <- function(.data){
  fields <- .data |>
    pluck("url") |>
    url_parse() |> 
    pluck("query", "fields") |>
    strsplit(",") |>
    pluck(1)
  media_fields <- c("images", "videos", "sounds")
  fields_check <- media_fields %in% fields
  if(!any(fields_check)){
    abort(c("No media fields requested",
            i = "Use `select()` to specify which media fields are required",
            i = "Valid fields are 'images', 'videos' and 'sounds'."),
          error_call = caller_env())
  }else{
    media_fields[fields_check]
  }
}
#' Internal function called by `filter()` et al
#' @noRd
#' @keywords Internal
check_named_input <- function(dots){
  name_length <- any(length(names(dots) > 0)) & any(names(dots) != "")
  if(name_length){
    bullets <- c(
      "We detected a named input.",
      i = "This usually means that you've used `=` instead of `==`.")
    abort(bullets)
  }
}

#' Check whether geolocate functions have >1 argument
#' @importFrom rlang warn
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

#' Internal function to ensure correct data extracted from API for LA/GBIF
#' @noRd
#' @keywords Internal
check_occurrence_response <- function(.data){
  if(is_gbif()){
    # list(
    #   completed_flag = "SUCCEEDED",
    #   queue_function = "check_queue_GBIF",
    #   queue_input = list(url = attr(.data, "url")),
    #   download_tag = "downloadLink",
    #   status_url = attr(.data, "url")
    # )
  }else{
    names(.data) <- camel_to_snake_case(names(.data))
    if(!is.null(.data$status_code)){
      switch(as.character(.data$status_code),
             "500" = {abort(c("There was a problem with your query.",
                              i = glue("message: {.data$message}")),
                            error_call = caller_env())},
             abort("Aborting for unknown reasons.", # FIXME
                   error_call = caller_env()))
    }else{
      if(.data$status == "finished"){
        .data$status <- "complete"
      }else{
        .data$status <- "incomplete"
      }
    }
  }
  .data
}

#' Internal function to change API response to contain standard headers
#' @noRd
#' @keywords Internal
check_occurrence_status <- function(.data){
  list(url = .data$status_url) |>
    query_API() |>
    as.list() |>
    check_occurrence_response()
}

#' Internal function to expand a url
#' Proposed to spin out multiple urls to paginate when n is high
#'  
#' Note: this needs to be in the compute stage of multiple APIs: ie. from `request_data()` and `request_metadata()`
#' Also requires something like `check_facet_count()` to know what the max value is.
#' @noRd
#' @keywords Internal
# check_pagination <- function(){}

#' Subfunction to `check_login()`
#' @noRd
#' @keywords Internal
check_password <- function(.data){
  if (.data$opts$userpwd == ":") {
    abort("GBIF requires a username and password to download occurrences or species.",
          call = error_call
    )
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

#' Internal function to check a supplied profile is valid
#' @noRd
#' @keywords Internal
check_profiles <- function(.data){
  if(!inherits(.data$url, "data.frame")){
    url <- strsplit(.data$url[1], "/")[[1]]
    profile <- url[length(url)]
    if(!profile %in% .data[["metadata/profiles"]]$shortName){
      abort(glue("profile `{profile}` not available for specified atlas"))
    }else{
      .data
    }
  }else{
    .data 
  }
}

#' Internal function to check that a reason code is valid
#' @noRd
#' @keywords Internal
check_reason <- function(.data, error_call = caller_env()){
  if(atlas_supports_reasons_api()) {
  if(.data$type %in% c("data/occurrences", "data/species")){
    query <- url_parse(.data$url)$query
    if(is.null(query$reasonTypeId)){
      bullets <- c("Missing a valid download reason.",
                   i = "Use `show_all(reasons)` to see all valid reasons.",
                   i = "Use `galah_config(download_reason_id = ...) to set a reason.")
      abort(bullets, call = error_call) 
    }else{
      value <- as.integer(query$reasonTypeId)
      if(!(value %in% .data[["metadata/reasons"]]$id)){
        bullets <- c(
          "Invalid download reason ID.",
          i = "Use `show_all(reasons)` to see all valid reasons.",
          x = glue("{value} does not match an existing reason ID."))
        abort(bullets, call = error_call)    
      }
    }
  }
  }
  .data
}

# Check for valid `type`
check_type_valid <- function(type, valid, error_call = caller_env()) {
  if(!any(valid == type)){
    bullets <- c(
      glue("Type `{type}` is not recognised."),
      i = "See ?show_all for a list of valid information types."
    )
    abort(bullets, call = error_call)   
  }
}
