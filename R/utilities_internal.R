##---------------------------------------------------------------
##                 Output formatting functions                 --
##---------------------------------------------------------------

# Select column names to return
# Subsets data returned by webservices to useful columns
wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", 
                      "taxon_concept_id", # ALA
                      "usage_key", # GBIF
                      "guid", # species search
                      "canonical_name", "status", 
                      "rank",
                      "match_type", "kingdom", "phylum", "class", "order",
                      "family", "genus", "species", "vernacular_name",
                      "issues","subkingdom", "superclass", "infraclass",
                      "subclass", "subinfraclass", "suborder", "superorder",
                      "infraorder", "infrafamily", "superfamily", "subfamily",
                      "subtribe", "subgenus", "subspecies"),
           "extended_taxa" = c("subkingdom", "superclass", "infraclass",
                               "subclass", "subinfraclass", "suborder",
                               "superorder", "infraorder", "infrafamily",
                               "superfamily", "subfamily", "subtribe",
                               "subgenus"),
           "checklist" = c("kingdom", "phylum", "class", "order", "family",
                           "genus", "species", "author", "species_guid",
                           "vernacular_name"),
           "profile" = c("id", "name", "shortName", "description"),
           "media" = c("media_id", "occurrence_id",
                       "creator", "license",
                       "data_resource_uid",
                       "date_taken", "date_uploaded",
                       "mime_type", "width", "height", "size_in_bytes"
                       # "image_url"
                     ),
           "layer" = c("layer_id", "description", "link"),
           "fields" = c("id", "description"),
           "assertions" = c("id", "description", "category"),
           "quality_filter" = c("description", "filter"),
           "reasons" = c("id", "name"))
}

# Rename specific columns, and convert to snake_case
rename_columns <- function(varnames, type) {
    if (type == "media") {
        varnames[varnames == "imageIdentifier"] <- "media_id"
    }
    else if (type == "taxa") {
        varnames[varnames == "classs"] <- "class"
        varnames[varnames == "usage_key"] <- "taxon_concept_id"
        varnames[varnames == "usageKey"] <- "taxon_concept_id"
        varnames[varnames == "guid"] <- "taxon_concept_id"
    } else if (type == "layer") {
        varnames[varnames == "displayname"] <- "name"
        varnames[varnames == "source_link"] <- "link"
    } else if (type == "fields") {
      varnames[varnames == "name"] <- "id"
      varnames[varnames == "info"] <- "description"
    } else if (type == "assertions") {
      varnames[varnames == "name"] <- "id"
    } else if (type == "checklist") {
      varnames[varnames == "Scientific.Name.Authorship"] <- "author"
      varnames[varnames == "Species"] <- "species_guid"
      varnames[varnames == "Species.Name"] <- "species"
    }
    # change all to snake case?
    if (type %in% c("taxa", "media")) {
        varnames <- tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", varnames,
                         perl = TRUE))
        varnames <- tolower(gsub("\\.", "_", varnames))
    } else if (type == "checklist") {
      varnames <- tolower(gsub("\\.", "_", varnames))
    } else if (type == "occurrence") {
      # change dots to camel case
      varnames <- gsub("\\.(\\w?)", "\\U\\1", varnames, perl = TRUE)
      # replace first letters with lowercase, but only if it is not an
      # all-uppercase field name (which assertions are)
      not_all_uppercase <- str_detect(varnames, "[[:lower:]]")
      substr(varnames[not_all_uppercase], 1, 1) <-
        tolower(substr(varnames[not_all_uppercase], 1, 1))
    }
    varnames
}

# Convert true/false to logical values
# Used in ala_occurrences output
fix_assertion_cols <- function(df, assertion_cols) {
  for (col in assertion_cols) {
    df[[col]] <- as.logical(df[[col]])
  }
  df
}


##----------------------------------------------------------------
##                   Parsing functions                          --
##----------------------------------------------------------------


# function to identify objects or functions in quosures, and eval them
# this is used by `search_taxa` and `galah_geolocate`
# It differs from functions in `galah_filter` by being more straightforward
parse_basic_quosures <- function(dots){
  is_either <- is_function_check(dots) | is_object_check(dots)
  result <- vector("list", length(dots))
  # If yes, evaluate them correctly as functions
  if(any(is_either)){
    result[is_either] <- lapply(dots[is_either], eval_tidy)
  }
  if(any(!is_either)){
    result[!is_either] <- lapply(dots[!is_either], 
      function(a){dequote(as_label(a))})
  }
  
  # return correct type
  if(check_taxize(result)){
    keep_class <- class(result[[1]])
    result <- unlist(do.call(as.character, result))
    class(result) <- paste0(keep_class, "+")
    return(result)
  }else if(check_character(result)){
    return(do.call(c, result))
  } else if(check_df(result)){
    return(do.call(rbind, result))
  } else {
    return(result)
  }
}

check_taxize <- function(x){ # where x is a list
  all(unlist(lapply(
    x,
    function(y){inherits(y, c("gbifid", "nbnid"))
  })))
}

check_character <- function(x){
  all(unlist(lapply(x, is.character)))
}

check_df <- function(x){
  all(unlist(lapply(x, is.data.frame)))
}

is_function_check <- function(dots){ # where x is a list of strings
  x <- unlist(lapply(dots, as_label))
  
  # detect whether function-like text is present
  functionish_text <- grepl("^(([[:alnum:]]|\\.|_)+\\()", x) & grepl("\\)", x)
  dollar_sign_square_bracket <- grepl("\\$|\\[", x)
  functions_present <- functionish_text | dollar_sign_square_bracket
  
  # if there are equations, that's only ok if they are quoted
  contains_equations <- grepl( "!=|>=|<=|==|>|<", x)
  quoted_equations <- grepl("(\"|\')\\s*(!=|>=|<=|==|>|<)\\s*(\"|\')", x) 
  equations_ok <- (contains_equations & quoted_equations) | !contains_equations
  # ...except where they start with the name `galah_`
  is_galah <- grepl("^galah_", x)
  
  # parse only if both conditions are met
  functions_present & (equations_ok | is_galah)
}

is_object_check <- function(dots){
  # get list of options from ?typeof & ?mode
  available_types <- c("logical", "numeric", 
    "complex", "character", "raw", "list", "NULL", "function",
    "name", "call", "any", "bbox")
  # attempt to check multiple types
  unlist(lapply(dots, function(a){
    modes_df <- data.frame(
      name = available_types,
      exists = unlist(lapply(available_types, function(b){
        exists(x = dequote(as_label(a)), envir = get_env(a), mode = b)
      }))
    )
    if(any(modes_df$exists)){
      if(all(modes_df$name[modes_df$exists] %in% c("any", "function"))){
        FALSE  # only functions exist here
      }else{
        TRUE # this avoids issues when a function and object are both valid
      }
    }else{
      FALSE # i.e. no objects
    }
  }))
}

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

##----------------------------------------------------------------
##                   Query-building functions                   --
##----------------------------------------------------------------

# Build query list from constituent arguments
build_query <- function(identify, filter, location, select = NULL,
                        profile = NULL) {
                          
  if (is.null(identify)) {
    if(taxon_key_type() == "GBIF"){
      taxa_query <- list(taxonKey = 1)
    }else{
      taxa_query <- NULL
    }
  } else { # assumes a tibble or data.frame has been given
    if(nrow(identify) < 1){
      taxa_query <- NULL
    } else {
      check_taxa_arg(identify)
      if (inherits(identify, "data.frame") &&
          "identifier" %in% colnames(identify)) {
        identify <- identify$identifier
      }
      #TODO: Implement a useful check here- i.e. string or integer
      # assert_that(is.character(taxa))
      taxa_query <- build_taxa_query(identify)
    }
  }
  
  # validate filters
  if (is.null(filter)) {
    filter_query <- NULL
  } else {
    assert_that(is.data.frame(filter))
    # remove profile from filter rows
    # filters <- filters[filters$variable != "profile",]
    if (nrow(filter) == 0) {
      filter_query <- NULL
    } else {
      filter_query <- build_filter_query(filter)
    }
  }
  
  if(taxon_key_type() == "GBIF"){
    query <- c(taxa_query, filter_query)
  }else{
    query <- list(fq = c(taxa_query, filter_query)) 
  } 
  
  # geographic stuff
  if (is.null(location)) {
    area_query <- NULL
  } else {
    area_query <- location
    query$wkt <- area_query
  }
  
  if (check_for_caching(taxa_query, filter_query, area_query, select)) {
    query <- cached_query(taxa_query, filter_query, area_query)
  }

  # add profiles information (ALA only)  
  if (getOption("galah_config")$atlas == "Australia") {
    if (!is.null(profile)) {
      query$qualityProfile <- profile
    } else {
      query$disableAllQualityFilters <- "true"
    }
  }
  
  query
}

# Build query from vector of taxonomic ids
build_taxa_query <- function(ids) {
  ids <- ids[order(ids)]
  if(taxon_key_type() == "GBIF"){
    list(taxonKey = ids)
  }else{
    glue(
      "(lsid:",
      glue_collapse(ids, sep = glue(" OR lsid:")),
      ")")
  }
}

# Takes a dataframe produced by galah_filter and return query as a list
# Construct individual query term
# Add required brackets, quotes to make valid SOLR query syntax
query_term <- function(name, value, include) {
  # add quotes around value
  value <- lapply(value, function(x) {
    # don't add quotes if there are square brackets in the term
    if (grepl("\\[", x)) {
      x
    } else {
      paste0("\"", x, "\"")
    }
  })
  # add quotes around value
  if (include) {
    value_str <- paste0("(", paste(name, value, collapse = " OR ", sep = ":"),
                        ")")
  } else {
    value_str <- paste0("-(", paste(name, value,
                                   collapse = " OR ", sep = ":"), ")")
  }
  value_str
}

old_query_term <- function(name, value, include) {
  # add quotes around value
  value <- lapply(value, function(x) {
    # don't add quotes if there are square brackets in the term
    if (grepl("\\[", x)) {
      x
    } else {
      paste0("\"", x, "\"")
    }
  })
  # add quotes around value
  if (include) {
    value_str <- paste0("(", paste(name, value, collapse = " OR ", sep = ":"),
                        ")")
  } else {
    value_str <- paste0("(", paste(paste0("-", name), value,
                                   collapse = " AND ", sep = ":"), ")")
  }
  value_str
}

build_filter_query <- function(filters) {
  if(getOption("galah_config")$atlas == "Global"){
    queries <- as.list(filters$value)
    names(queries) <- filters$variable
    queries
  }else{
    queries <- unique(filters$query)
    paste0(queries, collapse = " AND ")
  }
}

new_build_filter_query <- function(filters) {
  if(nrow(filters) > 1){
    query <- paste(
      apply(
        filters[, c("query", "join")], 
        1, 
        function(a){paste0(a, collapse = "")
      }),
     collapse = "")
    query <- sub("NA$", "", query)
  }else{
    query <- filters$query
  }
  return(query)
}

# Extract profile row from filters dataframe created by galah_filter
extract_profile <- function(filters) {
  profile <- NULL
  if (!is.null(filters)){
    profile <- attr(filters, "dq_profile")
  }
  profile
}

# Replace logical R values with strings
filter_value <- function(val) {
  if (is.logical(val)) {
    return(ifelse(val, "true", "false"))
  }
  val
}

# Construct string of column
build_columns <- function(col_df) {
  if (nrow(col_df) == 0) {
    return("")
  }
  paste0(col_df$name, collapse = ",")
}

build_assertion_columns <- function(col_df) {
  if (nrow(col_df) == 0) {
    return("none")
    # all assertions have been selected
  } else if (nrow(col_df) == 107) {
    return("includeall")
  }
  paste0(col_df$name, collapse = ",")
}

##---------------------------------------------------------------
##                   Query-caching functions                   --
##---------------------------------------------------------------

# Check whether caching of some url parameters is required.
# Note: it is only possible to cache one fq so filters can't be cached
check_for_caching <- function(taxa_query, filter_query, area_query,
                              columns = NULL, error_call = caller_env()) {
  if (nchar(paste(filter_query, collapse = "&fq=")) > 1948) {
    abort("Too many filters provided.", call = error_call)
  }
  if (sum(nchar(taxa_query), nchar(filter_query), nchar(area_query),
          nchar(paste(columns$name, collapse = ",")), na.rm = TRUE) > 1948) {
    # caching of taxa query and area query required
    return(TRUE)
  }
  return(FALSE)
}

# Cache a long query 
# Returns a query id (qid) from the ALA, which can then be used to reference a
# long query
cached_query <- function(taxa_query, filter_query, area_query,
                         columns = NULL) {
  resp <- atlas_url("records_query") |> 
          atlas_POST(body = list(
            wkt = area_query, 
            fq = taxa_query,
            fields = columns))
  list(fq = filter_query, q = paste0("qid:", resp))
}


##---------------------------------------------------------------
##                   Other helpful functions                   --
##---------------------------------------------------------------

# Construct the user agent string, consisting of the galah version
# This is added on to all requests to enable usage monitoring 
galah_version_string <- function() {
  version_string <- "version unknown"
  suppressWarnings(
    try(version_string <- utils::packageDescription("galah")[["Version"]],
        silent = TRUE)) ## get the galah version, if we can
  paste0("galah ", version_string)
}

# Check taxonomic argument provided to `atlas_` functions is of correct form
check_taxa_arg <- function(taxa, error_call = caller_env()) {
  # if (!any(grepl("id", class(taxa)))) {
  if(!inherits(taxa, "galah_identify")){
    if (!any(grepl("\\d", taxa))) {
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

##----------------------------------------------------------------
##                   Caching helper functions                   --
##----------------------------------------------------------------

# Read cached file
read_cache_file <- function(filename) {
  if (getOption("galah_config")$verbose) {
    inform(glue("Using cached file \"{filename}\"."))
  }
  readRDS(filename)
}

# Write file to cache and metadata to metadata cache
write_cache_file <- function(object, data_type, cache_file) {
  if (getOption("galah_config")$verbose) {
    inform(glue("
                
                Writing to cache file \"{cache_file}\".
                
                "))
    }
  tryCatch({
    saveRDS(object, cache_file)
    write_metadata(attributes(object)$data_request, data_type, cache_file)
    },
    error = function(e) {
      directory <- dirname(cache_file)
      bullets <- c(
        "There was an error writing to the cache file.",
        x = glue("Cache directory \"{directory}\" does not exist.")
      )
      warn(bullets)
    }
  )
}

# Hash cache filename from argument list
cache_filename <- function(...) {
  args <- c(...)
  filename <- paste0(digest(sort(args)), ".rds")
  file.path(getOption("galah_config")$cache_directory, filename)
}

# Write function call metadata to RDS file to enable metadata viewing with
# `find_cached_files()`
write_metadata <- function(request, data_type, cache_file) {
  metadata_file <- file.path(getOption("galah_config")$cache_directory,
                             "metadata.rds")
  if (file.exists(metadata_file)) {
    metadata <- readRDS(metadata_file)
  } else {
    metadata <- list()
  }
  file_id <- str_split(basename(cache_file), "\\.")[[1]][1]
  metadata[[file_id]] <- list(data_type = data_type, data_request = request)
  tryCatch(
    saveRDS(metadata, metadata_file),
    error = function(e) {
      directory <- dirname(cache_file)
      bullets <- c(
        "There was an error writing to the cache file.",
        x = glue("Cache directory \"{directory}\" does not exist.")
      )
      warn(bullets)
    }
  )
}

##----------------------------------------------------------------
##                   Request helper functions                   --
##----------------------------------------------------------------

# build_fq_url <- function(url, path, params = list()) {
#   url <- parse_url(url)
#   url$path <- path
#   url$query <- params[names(params) != "fq"]
#   join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
#   fq <- paste(params$fq, collapse = "&fq=")
#   paste0(build_url(url), join_char, URLencode(fq))
# }

build_fq_url <- function(url, params = list()) {
  url <- parse_url(url)
  if(any(names(params) == "fq")){
    # join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
    
    # ensure all arguments from galah_filter are enclosed in brackets
    fq <- params$fq
    missing_brackets <- !grepl("^\\(", fq)
    if(any(missing_brackets)){
      fq[missing_brackets] <- paste0("(", fq[missing_brackets], ")")
    }
    fq_single <- paste(fq, collapse = "AND")
    url$query <- c(fq = fq_single, params[names(params) != "fq"])
    build_url(url)
  }else{
    build_url(url)
  }
}

##---------------------------------------------------------------
##                Data request helper functions                --
##---------------------------------------------------------------

# Merge arguments 
merge_args <- function(request, extra) {
  # get non-null arguments
  non_null_request <- request[!unlist(lapply(request, is.null))]
  c(non_null_request, extra)
}

##---------------------------------------------------------------
##             show_all_ & search_ internal functions          --
##---------------------------------------------------------------

## show_all() & search_all()

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


## show_all_atlases / search_atlases --------------------------#

image_fields <- function() {
  atlas <- getOption("galah_config")$atlas
  switch (atlas,
          "Austria" = "all_image_url",
          "Guatemala" = "all_image_url",
          "Spain" = "all_image_url",
          c("images", "videos", "sounds")
  )
}

default_columns <- function() {
  atlas <- getOption("galah_config")$atlas
  switch (atlas,
          "Guatemala" = c("latitude", "longtitude", "species_guid",
                          "data_resource_uid", "occurrence_date", "id"),
          c("decimalLatitude", "decimalLongitude", "eventDate",
            "scientificName", "taxonConceptID", "recordID", "dataResourceName",
            "occurrenceStatus")
  )
}

species_facets <- function(){
  atlas <- getOption("galah_config")$atlas
  
  switch(atlas,
         "Australia" = "speciesID",
         # "Austria" = "species_guid",
         # "Brazil" = "species_guid",
         # "Canada" = "species_guid"
         "species_guid"
  )
}

# taxon_key_type <- function(){
#   atlas <- getOption("galah_config")$atlas
#   if(any(atlas == c("Estonia"))){
#     "GBIF"
#   }else{
#     "ALA"
#   }  
# }

taxon_key_type <- function(){"ALA"}


## show_all_fields --------------------------#


# Helper functions to get different field classes
get_fields <- function() {
  fields <- all_fields()
  if(is.null(fields)){
    NULL
  }else{
    # remove fields where class is contextual or environmental
    fields <- fields[!(fields$classs %in% c("Contextual", "Environmental")),]
    
    names(fields) <- rename_columns(names(fields), type = "fields")
    fields <- fields[wanted_columns("fields")]
    fields$type <- "fields"
    
    fields
  }
}

get_layers <- function() {
  url <- atlas_url("spatial_layers", quiet = TRUE)
  result <- atlas_GET(url)
  
  if(is.null(result)){
    NULL
  }else{
    if(all(c("type", "id") %in% names(result))){
      layer_id <- mapply(build_layer_id, result$type, result$id,
                         USE.NAMES = FALSE)
      result <- cbind(layer_id, result)
      result$description <- apply(
        result[, c("displayname", "description")],
        1,
        function(a){paste(a, collapse = " ")}
      )
      names(result) <- rename_columns(names(result), type = "layer")
      result <- result[wanted_columns("layer")]
      names(result)[1] <- "id"
      result$type <- "layers"
      result
    }else{
      NULL
    }
  }
}

# Return fields not returned by the API
get_other_fields <- function() {
  data.frame(id = "qid", description = "Reference to pre-generated query",
             type = "other")
}

# There is no API call to get these fields, so for now they are manually
# specified
get_media <- function(x) {
  
  ## Original code showed fields returned by `show_all_media`
  ## These can't be queried with `galah_filter` and have been replaced
  # fields <- data.frame(id = c("imageId", "height", "width", "tileZoomLevels",
  #                             "thumbHeight", "thumbWidth", "filesize", "mimetype",
  #                             "creator", "title", "description", "rights",
  #                             "rightsHolder", "license", "imageUrl", "thumbUrl",
  #                             "largeThumbUrl", "squareThumbUrl", "tilesUrlPattern"))
  data.frame(
    id = c("multimedia", "multimediaLicence", "images", "videos", "sounds"),
    description = "Media filter field",
    type = "media"
  )
}

all_fields <- function() {
  url <- atlas_url("records_fields")
  atlas_GET(url)
}

build_layer_id <- function(type, id) {
  if (type == "Environmental") {
    paste0("el", id)
  } else {
    paste0("cl", id)
  }
}


##---------------------------------------------------------------
##                      System down message                    --
##---------------------------------------------------------------

system_down_message <- function(function_name){
  bullets <- c(
    glue("Calling the API failed for `{function_name}`."),
    i = "This might mean that the API is down",
    i = "Double check that your query is correct, or try again later"
  )
  inform(bullets)
}

