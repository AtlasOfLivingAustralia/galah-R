#' Internal function to check whether first object is of class `data_request`
#' Called exclusively by `atlas_` functions
#' @noRd
#' @keywords Internal
check_atlas_inputs <- function(args){
  if(!is.null(args$request)){
    check_data_request(args$request)
    update_data_request(args$request, args[-1])
  }else{
    do.call(galah_call, args)
  }
}

#' Internal function to check for `data_request`s
#' @noRd
#' @keywords Internal
check_data_request <- function(request, error_call = caller_env()){
  if(!inherits(request, "data_request")){
    bullets <- c(
      "Argument `.query` requires an object of type `data_request`.",
      i = "You can create this object using `galah_call()`.",
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
    cache_dir
  }else{
    # if what is being tested is a file path, this will return false
    # if it is a file name (presumably appended to a path) this will return true
    directory <- ifelse(grepl("\\.[[:alpha:]]{2,4}$", x),
                        dirname(x),
                        x)
    if(!dir.exists(directory)){
      dir.create(directory, recursive = TRUE)
    }
    directory
  }
}

#' Internal function to ensure a download file is given
#' @noRd
#' @keywords Internal
check_download_filename <- function(file, ext = "zip"){
  if(!is.null(file)){ # is `file` present
    expected_suffix <- paste0(".", ext, "$")
    if(!grepl(expected_suffix, file)){ # expected suffix is missing
      if(grepl("\\.[[:alpha:]]{2,4}$", file)){ # does it have a different suffix?
        file <- gsub("\\.[[:alpha:]]{2,4}$", 
                     sub("\\$$", "", expected_suffix), 
                     file) # replace
      }else{
        file <- paste0(file, ".zip")  # append
      }
    } # no else{}, as all good here
  }else{
    current_time <- Sys.time() |> format("%Y-%m-%d_%H-%M-%S")
    file <- paste0('data_', current_time, ".", ext)
  }
  cache_directory <- pour("package", "directory", .pkg = "galah")
    glue("{cache_directory}/{file}") |>
      as.character()
    # check_path()? # currently commented out in check.R
}

#' Subfunction to `check_login()`
#' @importFrom jsonlite fromJSON
#' @noRd
#' @keywords Internal
check_email <- function(.query){
  if(is_gbif()){
    email_text <- fromJSON(.query$body)$notificationAddresses
  }else{
    email_text <- url_parse(.query$url)$query$email
  }
  if(is.null(email_text)) {
    abort_email_missing()
  }else if(email_text == ""){
    abort_email_missing()
  }
  .query
}

#' Check files are filtered properly
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_files_filter <- function(x){
  if(!(x$variable %in% c("media"))){
    abort("Variable name must be a valid `type` accepted by `request_files()`.")
  }
  if(!inherits(x$data, "data.frame")){
    abort("rhs must be a `tibble` containing media information")
  }
}

#' check that objects passed within `galah_filter` have correct structure
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
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
check_fields <- function(.query) {
  
  if(pour("package", "run_checks")){
    if(is_gbif()){
      if(.query$type == "data/occurrences"){
        check_result <- check_fields_gbif_predicates(.query)  
      }else{
        check_result <- check_fields_gbif_counts(.query)
      }
    }else{
      check_result <- check_fields_la(.query)
    }

    # error message
    if(any(!is.na(check_result))) {
      returned_invalid <- tibble(
        function_name = c("`galah_filter()`", "`galah_group_by()`"),
        fields = check_result
      ) |>
        drop_na()
      
      glue_template <- "{returned_invalid$function_name}: {returned_invalid$fields}"
      invalid_fields_message <- glue_data(returned_invalid, glue_template, .na = "")
      
      bullets <- c(
        "Can't use fields that don't exist.",
        i = "Use `search_all(fields)` to find a valid field ID.",
        x = glue("Can't find field(s) in"),
        glue("  ", format_error_bullets(invalid_fields_message))
      )
      abort(bullets)
    }
  }
  .query
}

#' sub-function to `check_fields()` for living atlases
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
check_fields_gbif_counts <- function(.query){
  # set fields to check against
  valid_fields <- .query[["metadata/fields"]]$id
  valid_assertions <- .query[["metadata/assertions"]]$id
  valid_any <- c(valid_fields, valid_assertions)
  url <- url_parse(.query$url[1])

  # get fields from url
  query_names <- names(url$query)
  fields <- query_names[!(query_names %in% 
                          c("limit", "facet", "facetLimit", "taxonKey"))] # note: taxonKey here as specified internally
  # check invalid fields
  filter_invalid <- NA
  if (length(fields) > 0) {
    if (!all(fields %in% valid_any)) {
      invalid_fields <- fields[!(fields %in% valid_any)]
      filter_invalid <- glue_collapse(invalid_fields, sep = ", ")
    }
  }
  
  # check for invalid facets
  group_by_invalid <- NA
  if(any(query_names == "facet")){
    fields <- unlist(url$query[which(query_names == "facet")])
    if (!all(fields %in% valid_any)) {
      invalid_fields <- fields[!(fields %in% valid_any)]
      group_by_invalid <- glue_collapse(invalid_fields, sep = ", ")
    }
  }
  
  c(filter_invalid, NA, group_by_invalid)
}

#' sub-function to `check_fields()` for living atlases
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
check_fields_gbif_predicates <- function(.query){
  # set fields to check against
  valid_fields <- .query[["metadata/fields"]]$id
  valid_assertions <- .query[["metadata/assertions"]]$id
  valid_any <- c(valid_fields, valid_assertions) |>
    camel_to_snake_case() |>
    toupper()
  # extract fields
  fields <- .query$body |> 
    fromJSON() |>
    pluck("predicate", "predicates", "key")
  # check invalid
  filter_invalid <- NA
  if (length(fields) > 0) {
    if (!all(fields %in% valid_any)) {
      invalid_fields <- fields[!(fields %in% valid_any)]
      filter_invalid <- glue_collapse(invalid_fields, sep = ", ")
    }
  }
  c(filter_invalid, NA, NA)
}

#' sub-function to `check_fields()` for living atlases
#' @noRd
#' @keywords Internal
check_fields_la <- function(.query){
  url <- url_parse(.query$url[1])
  queries <- url$query
  
  # set fields to check against
  # NOTE: These are retrieved in collapse()
  valid_fields <- .query[["metadata/fields"]]$id
  valid_assertions <- .query[["metadata/assertions"]]$id
  valid_any <- c(valid_fields, valid_assertions)
  
  # extract fields from filter & identify
  filter_invalid <- NA
  if(is.null(queries$fq)){
    # note: above was previously: `exists("fq", where = queries)`
    # Error in as.environment(where) : using 'as.environment(NULL)' is defunct
    filters <- NULL
  }else{
    if (nchar(queries$fq) > 0) {
      provided_fields <- string_to_tibble(queries$fq)
      filters <- provided_fields |>
        pull("value") |>
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
  
  c(filter_invalid, group_by_invalid)
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
#' @noRd
#' @keywords Internal
check_identifiers <- function(.query){
  if(is_gbif()){
    if(.query$type %in% c("data/occurrences", "data/species")){
      check_identifiers_gbif_predicates(.query)
    }else{
      check_identifiers_gbif(.query) # mainly for `data/occurrences-count` 
    }
  }else{
    check_identifiers_la(.query)
  }
}

#' `check_identifiers()` for gbif
#' @noRd
#' @keywords Internal
check_identifiers_gbif <- function(.query){
  url <- url_parse(.query$url[1]) # FIXME: test if every >1 urls here
  if(!is.null(url$query)){
    metadata_lookup <-grepl("metadata/taxa", names(.query))
    if(any(metadata_lookup)){
      identifiers <- .query[[which(metadata_lookup)[1]]]
      taxon_query <- as.list(identifiers$taxon_concept_id)
      names(taxon_query) <- rep("taxonKey", length(taxon_query))
      url$query <- c(taxon_query,  
                     url$query[names(url$query) != "taxonKey"])
      .query$url[1] <- url_build(url)
    }
  }
  .query
}

#' `check_identifiers()` for gbif occurrences
#' @noRd
#' @keywords Internal
check_identifiers_gbif_predicates <- function(.query){
  if(!is.null(.query$body)){
    metadata_lookup <-grepl("metadata/taxa", names(.query))
    if(any(metadata_lookup)){
      identifiers <- .query[[which(metadata_lookup)[1]]]
      .query$body <- sub("`TAXON_PLACEHOLDER`", 
                        identifiers$taxon_concept_id[1], 
                        .query$body)
    }
  }
  .query
}
  
#' `check_identifiers()` for living atlases 
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @importFrom rlang abort
#' @importFrom stringr str_replace_all
#' @noRd
#' @keywords Internal
check_identifiers_la <- function(.query){
  url <- url_parse(.query$url[1]) # FIXME: test if every >1 urls here
  queries <- url$query
  if(!is.null(queries$fq)){
    if(grepl("(`TAXON_PLACEHOLDER`)", queries$fq)){
      metadata_lookup <- grepl("^metadata/taxa", names(.query))
      if(any(metadata_lookup)){
        identifiers <- .query[[which(metadata_lookup)[1]]]
        taxa_ids <- build_taxa_query(identifiers$taxon_concept_id)
        queries$fq <- str_replace_all(queries$fq, 
                                      "\\(`TAXON_PLACEHOLDER`\\)", 
                                      taxa_ids)
        url$query <- queries
        .query$url[1] <- url_build(url)
      }else{
        # this only happens if there is a bug earlier in the code
        abort("The query has a taxonomic placeholder, but no taxon search has been run.")
      }
    }
  }else{
    # note: `metadata/taxa-unnest` parses here
    if(grepl("%60TAXON_PLACEHOLDER%60", .query$url[1])){
      metadata_lookup <- grepl("^metadata/taxa", names(.query))
      if(any(metadata_lookup)){ 
        identifiers <- .query[[which(metadata_lookup)[1]]]
        taxa_id <- identifiers$taxon_concept_id[1]
        .query$url[1] <- sub("%60TAXON_PLACEHOLDER%60", taxa_id, .query$url[1])
      }else{
        abort("The query has a taxonomic placeholder, but no taxon search has been run.")
      }
    }
  }
  .query
}

#' Internal function to confirm requisite login information has been provided
#' Called by `compute()`
#' @noRd
#' @keywords Internal
#' @importFrom rlang caller_env
check_login <- function(.query, error_call = caller_env()) {
  # Check for valid email for occurrences or species queries for all providers
  if(.query$type == "data/occurrences" | .query$type == "data/species"){
    switch(pour("atlas", "region"), 
           "United Kingdom" = {},
           "Global" = {check_email(.query); check_password(.query)},
           check_email(.query))
  }
  .query
  # ALA requires an API key
  # } else if (pour("atlas", "acronym") == "ALA") {
  #   if (is.null(.query$headers$`x-api-key`) | .query$headers$`x-api-key` == "") {
  #     abort_api_key_missing()
  #   }
  # }  
}

#' Internal function to convert multi-value media fields to list-columns
#' @param .query A tibble() returned by atlas_occurrences
#' @noRd
#' @keywords Internal
check_media_cols <- function(.query){
  media_colnames <- c("images", "sounds", "videos")
  # if media columns are not present, return original data unchanged
  if(!any(colnames(.query) %in% media_colnames)){
    .query
  }
  # otherwise get media columns
  present_cols <- media_colnames[media_colnames %in% colnames(.query)]
  for(i in present_cols){
    if(!all(is.na(.query[[i]]))){
      .query[[i]] <- strsplit(.query[[i]], "\\s\\|\\s")
    }
  }
  .query
}

#' Internal function to check whether valid media fields have been supplied
#' @param .query a `query` object
#' @noRd
#' @keywords Internal
check_media_cols_present <- function(.query, error_call = caller_env()){
  fields <- .query |>
    pluck("url") |>
    url_parse() |> 
    pluck("query", "fields") |>
    strsplit(",") |>
    pluck(1)
  media_fields <- c("images", "videos", "sounds")
  fields_check <- media_fields %in% fields
  if(!any(fields_check)){
    abort(c("No media fields requested.",
            i = "Use `select()` to specify which media fields are required.",
            i = "Valid fields are 'images', 'videos' and 'sounds'."),
          call = error_call)
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
#' It makes all calls consistent so we only need one queue checking function
#' @noRd
#' @keywords Internal
check_occurrence_response <- function(.query){
  
  names(.query) <- camel_to_snake_case(names(.query))
  
  if (!is.null(.query$status_code)) {
    
    error_type <- sub("\\:.*", "", .query$message) |> stringr::str_trim()
    
    bullets <- c(
      "There was a problem with your query.",
      "*" = glue("message: {.query$message}"))
    
    switch(as.character(error_type),
           "500" = {abort(bullets,
                          call = caller_env())},
           "403" = {abort(c(bullets,
                          i = "Is the email you provided to `galah_config()` registered with the selected atlas?"),
                          call = caller_env())},
           "404" = {abort(c(bullets,
                          i = "Is the email you provided to `galah_config()` registered with the selected atlas?"),
                          call = caller_env())},
           "504" = {abort(c(bullets,
                          i = "This usually means that the selected API is down.",
                          i = "If you continue to receive this error, please email support@ala.org.au"),
                          call = caller_env())},
           abort("Aborting for unknown reasons.", # FIXME
                 call = caller_env()))
  } else {
    if (.query$status %in% c("finished", # ALA
                            "SUCCEEDED") # GBIF
    ){
      .query$status <- "complete"
    } else {
      .query$status <- "incomplete"
    }
  }
  # harmonisation of GBIF objects
  # change name of `download_link` to `download_url`
  check_download_link <- names(.query) == "download_link"
  if(any(check_download_link)){
    names(.query)[which(check_download_link)[1]] <- "download_url"
  }
  # convert `key` to `status_url`
  if(is.null(.query$status_url) & !is.null(.query$key)){
    .query$status_url <- paste0("https://api.gbif.org/v1/occurrence/download/",
                               .query$key)
  }
  # add `queue_size`
  if(is.null(.query$queue_size)){
    .query$queue_size <- 0
  }
  .query
}

#' Internal function to change API response to contain standard headers
#' @noRd
#' @keywords Internal
check_occurrence_status <- function(.query){
  list(url = .query$status_url) |>
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
check_password <- function(.query, 
                           error_call = caller_env()){
  if (.query$options$userpwd == ":") {
    abort("GBIF requires a username and password to download occurrences or species.",
          call = error_call)
  }
}

# Internal function to create a valid filename for download
# Note this is most commonly used when galah defaults are in place; i.e. 
# downloads are sent to a temporary directory.
# Called by `query_API()`
# check_path <- function(.query){
#   if(is.null(.query$path)){
#     if(.query$type == "species"){
#       ext <- "csv"
#     }else{
#       ext <- "zip"
#     }
#     cache_file <- pour("package", "directory")
#     .query$path <- paste0(cache_dir, "/temp_file.", ext)    
#   } else {
#     dirname(x) |> check_directory() # errors if path doesn't exist
#     # NOTE: it might make sense here to check that a supplied filename is valid
#   }
#   .query
# }

#' Internal function to check a supplied profile is valid
#' @importFrom glue glue
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_profiles <- function(.query, error_call = caller_env()){
  if(!inherits(.query$url, "data.frame")){
    query <- url_parse(.query$url[1])$query
    if(!is.null(query$qualityProfile)){
      profile <- query$qualityProfile
      if(!profile %in% .query[["metadata/profiles"]]$shortName){
        bullets <- c(
          "Unrecognised profile requested.",
          i = "See `?show_all(profiles)` for valid profiles.",
          x = glue("Can't find profile `{profile}` for specified atlas."))
        abort(bullets, call = error_call)
      }else{
        .query
      }
    }else{
      .query
    }
  }else{
    .query 
  }
}

#' Internal function to check that a reason code is valid
#' @importFrom glue glue
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_reason <- function(.query, error_call = caller_env()){
  if(atlas_supports_reasons_api()) {
    if(.query$type %in% c("data/occurrences", "data/species")){
      query <- url_parse(.query$url)$query
      if(is.null(query$reasonTypeId)){
        bullets <- c("Missing a valid download reason.",
                     i = "See `show_all(reasons)`.",
                     i = "Use `galah_config(download_reason_id = ...)` to set a download reason.")
        abort(bullets, call = error_call) 
      }else{
        user_reason <- query$reasonTypeId
        valid_reasons <- .query[["metadata/reasons"]]$id
        if(!(user_reason %in% valid_reasons)){
          bullets <- c(
            "Invalid download reason ID.",
            i = "Use `show_all(reasons)` to see all valid reasons.",
            x = glue("\"{user_reason}\" does not match an existing reason ID."))
          abort(bullets, call = error_call)    
        }
      }
    }
  }
  .query
}

#' Check that `select()` quosures can be parsed correctly
#' NOTE: much of this content was previously in `parse_select()` (defunct)
#' @importFrom dplyr all_of
#' @importFrom dplyr filter
#' @importFrom httr2 url_parse
#' @importFrom httr2 url_build
#' @importFrom rlang is_quosure
#' @importFrom tidyselect eval_select
#' @noRd
#' @keywords Internal
check_select <- function(.query){
  if(any(names(.query) == "select")){
    if(is_gbif()){
      inform(c("skipping `select()`:",
               i = "This function is not supported by the GBIF API v1"))
    }else{
      # 1. build df to `select` from
      valid_fields <- .query[["metadata/fields"]]$id
      valid_assertions <- .query[["metadata/assertions"]]$id
      valid_any <- c(valid_fields, valid_assertions)
      df <- matrix(data = NA, nrow = 0, ncol = length(valid_any),
                   dimnames = list(NULL, valid_any)) |>
        as.data.frame()

      # 2. parse groups
      group_initial <- .query$select$group
      # new step to avoid calling `show_all_assertions()` internally
      group <- group_initial[group_initial != "assertions"]
      if(length(group) > 0){
        group_cols <- lapply(group, preset_groups) |> 
          unlist()
        group_names <- eval_select(all_of(group_cols), data = df) |> 
          names()
        # note: technically `group_names` and `group_cols` are identical
        # BUT `eval_select()` will fail if invalid columns are given
      }else{
        group_names <- NULL
      }
      
      # 3. parse quosures to get list of field names
      check_quosures <- lapply(.query$select, is_quosure) |>
        unlist()
      dots <- .query$select[check_quosures]
      dot_names <- lapply(dots, function(a){
        eval_select(a, data = df) |>
          names()
      }) |>
        unlist()

      # 3a: set 'identifier' column name
      if(pour("atlas", "region") == "United Kingdom"){
        id_col <- "id"
      }else{
        id_col <- "recordID"
      }
      
      # 4: set behaviour depending on what names are given
      # NOTE:
      ## because assertions aren't fields, leaving `fields` empty means default fields are returned
      ## but only when `group = assertions` and no other requests are made
      ## this adds a single field (recordID) to the query to avoid this problem.
      ## This problem also occurs when a single field is requested
      ## under some circumstances (e.g. "images"), even when that field is 
      ## fully populated.
      if(length(dot_names) > 1){
        individual_cols <- dot_names
      }else{ 
        if(length(dot_names) == 1){ # i.e. a single field selected
          if(length(group_names) == 0){
            individual_cols <- unique(c(id_col, dot_names))
          }else{
            individual_cols <- dot_names
          }
        }else{ # i.e. length(dot_names) == 0, meaning no fields selected
          if(length(group_initial) <= 1 & !any(group_names == id_col)){
            individual_cols <- id_col
          }else{
            individual_cols <- NULL
          }
        }
      }
      
      # 5. merge to create output object
      # NOTE: placing `recordID` first is critical;
      # having e.g. media columns _before_ `recordID` causes the download to fail 
      field_values <- unique(c(group_names, individual_cols))
      if(is.null(field_values)){
        bullets <- c("No fields selected",
                     i = "Please specify a valid set of fields in `select()`",
                     i = "You can look up valid fields using `show_all(fields)`")
        abort(bullets)
      }
      if(any(field_values == id_col)){
        field_values <- c(id_col, field_values[field_values != id_col]) # recordID needs to be first
      }
      
      # 6. handle assertions
      is_assertion <- field_values %in% valid_assertions
      if(any(group_initial == "assertions")){
        assertion_text <- "includeall"
      }else{
        if(any(is_assertion)){
          assertion_text <- paste(field_values[is_assertion], collapse = ",")
        }else{
          assertion_text <- "none"
        }
      }
      field_text <- paste(field_values[!is_assertion],
                          collapse = ",")
      
      # 7. replace `SELECT_PLACEHOLDER` with valid query
      # located in .query$url in query/fields
      url <- url_parse(.query$url) # note: this assumes a single url every time
      url$query$fields <- field_text
      url$query$qa <- assertion_text
      .query$url <- url_build(url)
      .query$select <- NULL
    }
  }
  .query
}

#' Check for valid `type`
#' @noRd
#' @keywords Internal
check_type_valid <- function(type, valid, error_call = caller_env()) {
  if(!any(valid == type)){
    bullets <- c(
      glue("Unrecognised metadata requested."),
      i = "See `?show_all()` for a list of valid metadata types.",
      x = glue("Can't find metadata type `{type}`.")
    )
    abort(bullets, call = error_call)   
  }
}
