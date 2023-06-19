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
                      "authority", # OpenObs
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
           "media" = c("media_id",
                       "creator", "license",
                       "data_resource_uid",
                       "date_taken", "date_uploaded",
                       "mime_type", "width", "height", "size_in_bytes",
                       "image_url"
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
        varnames[varnames %in% c("usage_key", "usageKey", "guid", "reference_id", "referenceId")] <- "taxon_concept_id"
        varnames[varnames %in% c("genus_name", "genusName")] <- "genus"
        varnames[varnames %in% c("family_name", "familyName")] <- "family"
        varnames[varnames %in% c("order_name", "orderName")] <- "order"
        varnames[varnames %in% c("class_name", "className")] <- "class"
        varnames[varnames %in% c("phylum_name", "phylumName")] <- "phylum"
        varnames[varnames %in% c("kingdom_name", "kingdomName")] <- "kingdom"
        varnames[varnames %in% c("rank_name", "rankName")] <- "rank"
        varnames[varnames %in% c("french_vernacular_name", "frenchVernacularName")] <- "vernacular_name"
    } else if (type == "layer") {
        varnames[varnames == "displayname"] <- "name"
        varnames[varnames == "source_link"] <- "link"
    } else if (type == "fields") {
      varnames[varnames == "name"] <- "id"
      varnames[varnames == "info"] <- "description"
    } else if (type == "assertions") {
      varnames[varnames == "name"] <- "id"
    } else if (type == "checklist") {
      varnames[varnames == "Scientific Name Authorship"] <- "author"
      varnames[varnames == "Species"] <- "species_guid"
      varnames[varnames == "Species Name"] <- "species"
    }
    # change all to snake case?
    if (type %in% c("taxa", "media")) {
        varnames <- tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", varnames,
                         perl = TRUE))
        varnames <- tolower(gsub("\\.", "_", varnames))
    } else if (type == "checklist") {
      varnames <- tolower(gsub("\\.|\\s", "_", varnames))
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
##                   Query-building functions                   --
##----------------------------------------------------------------

#' Build query list from constituent arguments
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
build_query <- function(identify, 
                        filter, 
                        location = NULL, 
                        profile = NULL) {
                          
  if (is.null(identify)) {
    if(is_gbif()){
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
    if (nrow(filter) == 0) {
      filter_query <- NULL
    } else {
      filter_query <- build_filter_query(filter)
    }
  }
  
  if(is_gbif()){
    query <- c(taxa_query, filter_query)
  }else{
    query <- list(fq = c(taxa_query, filter_query)) 
  } 
  
  # geographic stuff
  if (!is.null(location)) {
    query$wkt <- location
  }

  # add profiles information (ALA only)  
  if(pour("atlas", "region") == "Australia"){
    if(!is.null(profile)) {
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
  if(is_gbif()){
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
  # check for blank value
  blank_value <- if(value == "") {TRUE} else {FALSE}

  # add quotes around value
  value <- lapply(value, function(x) {
    # don't add quotes if there are square brackets in the term, 
    # or if there are no spaces
    # if (grepl("\\[|\\s", x)) {
    #   x
    # } else {
    #   paste0("\"", x, "\"")
    # }
    rlang::expr_text(value) # format query value as solr-readable text
  })
  
  # add quotes around value
  if(isTRUE(blank_value)) {
    value_str <- parse_blank_query(name, include)
  } else {
    if (include) {
      value_str <- paste0("(", 
                          paste(name, value, collapse = " OR ", sep = ":"),
                          ")")
    } else {
      value_str <- paste0("-(", 
                          paste(name, value, collapse = " OR ", sep = ":"), 
                          ")")
    }
  }
  value_str
}

# Specific query building for queries with "blank" statements
# e.g. eventDate == ""
# solr equivalent to searching for NAs
parse_blank_query <- function(name, include) {
  if (include) { 
    value_str <- paste0("(*:* AND -", name, ":*)") # queries with "=="
  } else { 
    value_str <- paste0("(", name, ":*)") # queries with "!="
  }
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
  if(is_gbif()){
    is_equals <- filters$logical == "=="
    if(any(is_equals)){
      filters$query[is_equals] <- filters$value[is_equals]
    }
    if(any(!is_equals)){
      cleaned_filters <- sub("^[[:graph:]]+\\[", "", 
                             x = filters$query[!is_equals])
      cleaned_filters <- sub("\\]$", "", x = cleaned_filters)
      cleaned_filters <- sub(" TO ", ",", x = cleaned_filters)
      filters$query[!is_equals] <- cleaned_filters
    }
    queries <- as.list(filters$query)
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
  assertion_group <- any(attr(col_df, "group") == "assertions")
  assertion_rows <- which(col_df$type == "assertion")
  if(assertion_group){ # assertions have been selected as a group
    if(length(assertion_rows) > 50){ # only if a certain number present
      return("includeall")
    }else{
      return(paste0(col_df$name[assertion_rows], collapse = ","))
    }
  }else{ # assertions not selected as a group
    if(length(assertion_rows) > 0) {
      return(paste0(col_df$name[assertion_rows], collapse = ","))
    }else{
      return("none")
    }
  }
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
  paste0("galah-R ", version_string)
}

#' Internal function for determining if we should call GBIF or not
#' @importFrom potions pour
#' @noRd
#' @keywords Internal
is_gbif <- function(){
  pour("atlas", "region") == "Global"
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


## show_all_atlases / search_atlases --------------------------#

image_fields <- function() {
  atlas <- pour("atlas", "region")
  switch (atlas,
          "Austria" = "all_image_url",
          "Guatemala" = "all_image_url",
          "Spain" = "all_image_url",
          c("images", "videos", "sounds")
  )
}

species_facets <- function(){
  atlas <- pour("atlas", "region")
  switch(atlas,
         "Australia" = "speciesID",
         # "Austria" = "species_guid",
         # "Brazil" = "species_guid",
         # "Canada" = "species_guid"
         "species_guid"
  )
}


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
    
    # correct id for recordID
    record_id_lookup <- grepl("http://rs.tdwg.org/dwc/terms/recordID", 
                              fields$description)
    if(any(record_id_lookup)){
      fields$id[which(record_id_lookup)[1]] <- "recordID"
    }
    
    return(tibble(fields))
  }
}

#' Get spatial layers
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @keywords Internal
#' @noRd
get_layers <- function() {
  url <- url_lookup("spatial_layers", quiet = TRUE)
  if(is.null(url)){
    return(NULL)
  }
  result <- url_GET(url)
  
  if(is.null(result)){
    NULL
  }else{
    result <- tibble(result)
    
    if(pour("atlas", "acronym") == "ALA"){
      result <- result |>
        filter(enabled == TRUE) |>
        select(id, desc) |>
        mutate(type = "layer")
      names(result)[2] <- c("description")
      return(result)
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
}

# Return fields not returned by the API
get_other_fields <- function() {
  tibble(id = "qid", description = "Reference to pre-generated query",
             type = "other")
}

# There is no API call to get these fields, so for now they are manually
# specified
get_media <- function(x) {
  tibble(
    id = c("multimedia", "multimediaLicence", "images", "videos", "sounds"),
    description = "Media filter field",
    type = "media"
  )
}

all_fields <- function() {
  url <- url_lookup("records_fields")
  url_GET(url)
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
    i = "This might mean that the API is down, or that you are not connected to the internet",
    i = "Double check that your query is correct, or try again later"
  )
  inform(bullets)
}

