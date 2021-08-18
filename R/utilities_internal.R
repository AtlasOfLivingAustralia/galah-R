##---------------------------------------------------------------
##                 Output formatting functions                 --
##---------------------------------------------------------------

# Select column names to return
# Subsets data returned by webservices to useful columns
wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", "taxon_concept_id", "rank",
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
           "media" = c("rightsHolder", "license", "creator", "title", "rights",
                       "mimetype", "media_id"),
           "layer" = c("layer_id", "description", "link"),
           "fields" = c("id", "description"),
           "assertions" = c("id", "description"),
           "quality_filter" = c("description", "filter"),
           "reasons" = c("id", "name"))
}

# Rename specific columns, and convert to snake_case
rename_columns <- function(varnames, type) {
    if (type == "media") {
        varnames[varnames == "imageId"] <- "media_id"
    }
    else if (type == "taxa") {
        varnames[varnames == "classs"] <- "class"
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
    if (type == "taxa") {
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
    df[, col] <- as.logical(df[, col])
  }
  df
}

##----------------------------------------------------------------
##                   Query-building functions                   --
##----------------------------------------------------------------

# Build query list from constituent arguments
build_query <- function(taxa, filters, locations, columns = NULL,
                        profile = NULL) {
  query <- list()
  if (is.null(taxa)) {
    taxa_query <- NULL
  } else {
    check_taxa_arg(taxa)
    # include <- !inherits(taxa, "exclude") # obsolete
    if (inherits(taxa, "data.frame") &&
        "taxon_concept_id" %in% colnames(taxa)) {
      taxa <- taxa$taxon_concept_id
    }
    #TODO: Implement a useful check here- i.e. string or integer
    # assert_that(is.character(taxa))
    taxa_query <- build_taxa_query(taxa)
  }
  
  # validate filters
  if (is.null(filters)) {
    filter_query <- NULL
  } else {
    assert_that(is.data.frame(filters))
    # remove profile from filter rows
    # filters <- filters[filters$variable != "profile",]
    if (nrow(filters) == 0) {
      filter_query <- NULL
    } else {
      filter_query <- build_filter_query(filters)
    }
  }
  
  query$fq <- c(taxa_query, filter_query)
  
  if (is.null(locations)) {
    area_query <- NULL
  } else {
    area_query <- locations
    query$wkt <- area_query
  }
  if (check_for_caching(taxa_query, filter_query, area_query, columns)) {
    query <- cached_query(taxa_query, filter_query, area_query)
  }
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
  # if (include) {
     value_str <- paste0("(lsid:", paste(ids, collapse = " OR lsid:"), ")")
  # } else {
  #   value_str <- paste0("(-lsid:", paste(ids, collapse = " AND -lsid:"), ")")
  # }
  value_str
}

# Takes a dataframe produced by select_filters and return query as a list
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
  queries <- unique(filters$query)
  paste0(queries, collapse = " AND ")
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

# Extract profile row from filters dataframe created by select_filters
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
                              columns = NULL) {
  if (nchar(paste(filter_query, collapse = "&fq=")) > 1948) {
    stop("Too many filters provided.")
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
  url <- server_config("records_base_url")
  resp <- ala_POST(url, path = "ws/webportal/params",
                   body = list(wkt = area_query, fq = taxa_query,
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

# Check taxonomic argument provided to `ala_` functions is of correct form
check_taxa_arg <- function(taxa) {
  if (!any(grepl("id", class(taxa)))) {
    if (!any(grepl("\\d", taxa))) {
      stop("`taxa` argument requires an identifier input generated by `select_taxa()` or a `taxize` function.")
    }
  }
}

##----------------------------------------------------------------
##                   Caching helper functions                   --
##----------------------------------------------------------------

# Read cached file
read_cache_file <- function(filename) {
  if (getOption("galah_config")$verbose) {
    message("Using cached file '", filename, "'")
  }
  readRDS(filename)
}

# Write file to cache and metadata to metadata cache
write_cache_file <- function(object, data_type, cache_file) {
  if (getOption("galah_config")$verbose) {
    message("Writing to cache file '", cache_file, "'")
    }
  tryCatch({
    saveRDS(object, cache_file)
    write_metadata(attributes(object)$data_request, data_type, cache_file)
    },
    error = function(e) {
      warning("There was an error writing to the cache file. Possibly the cache directory '",
              dirname(cache_file), "' doesn't exist.")
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
      warning("There was an error writing to the cache metadata. Possibly the cache directory ",
              dirname(cache_file), " doesn't exist.")
    }
  )
}

##----------------------------------------------------------------
##                   Request helper functions                   --
##----------------------------------------------------------------

build_fq_url <- function(url, path, params = list()) {
  url <- parse_url(url)
  url$path <- path
  url$query <- params[names(params) != "fq"]
  join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
  fq <- paste(params$fq, collapse = "&fq=")
  paste0(build_url(url), join_char, URLencode(fq))
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
