wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", "taxon_concept_id", "rank",
                      "match_type", "kingdom", "phylum", "class", "order",
                      "family", "genus", "species", "vernacular_name",
                      "issues"),
           "profile" = c("id", "name", "shortName", "description"),
           "media" = c("rightsHolder", "imageIdentifier", "format",
                       "occurrenceID", "recognisedLicence", "license",
                       "creator", "title", "rights", "mimeType",
                       "media_id"),
           "layer" = c("layer_id", "description", "link"),
           "fields" = c("id", "description"),
           "assertions" = c("id", "description"),
           "quality_filter" = c("description", "filter"),
           "reasons" = c("id", "name"))
}

# rename specific columns, and convert camelCase to snake_case
rename_columns <- function(varnames, type) {
    if (type == "media") {
        varnames[varnames == "mimeType"] <- "format"
        varnames[varnames == "imageIdentifier"] <- "media_id"
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
    }
    # change all to snake case?
    if (type == "taxa") {
        varnames <- tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", varnames,
                         perl = TRUE))
    } else if (type == "checklist") {
      varnames <- tolower(gsub("\\.", "_", varnames))
    } else if (type == "occurrence") {
      # change dots to camel case
      varnames <- gsub("\\.(\\w?)", "\\U\\1", varnames, perl = TRUE)
      # replace first letters with lowercase
      substr(varnames, 1, 1) <- tolower(substr(varnames, 1, 1))
    }
    varnames
}

build_taxa_query <- function(ids, include) {
  ids <- ids[order(ids)]
  if (include) {
    value_str <- paste0("(lsid:", paste(ids, collapse = " OR lsid:"), ")")
  } else {
    value_str <- paste0("(-lsid:", paste(ids, collapse = " AND -lsid:"), ")")
  }
  value_str
}

build_query <- function(taxa, filters, locations, columns = NULL) {
  
  query <- list()
  if (is.null(taxa)) {
    taxa_query <- NULL
  } else {
    include <- !inherits(taxa, "exclude")
    if (inherits(taxa, "data.frame") &&
        "taxon_concept_id" %in% colnames(taxa)) {
      taxa <- taxa$taxon_concept_id
    }
    assert_that(is.character(taxa))
    taxa_query <- build_taxa_query(taxa, include)
  }
  
  # validate filters
  if (is.null(filters)) {
    filter_query <- NULL
  } else {
    assert_that(is.data.frame(filters))
    filter_query <- build_filter_query(filters)
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
  query
}

# this is only relevant for ala_counts and ala_occurrences
cached_query <- function(taxa_query, filter_query, area_query,
                         columns = NULL) {
  url <- getOption("galah_server_config")$base_url_biocache
  resp <- ala_POST(url, path = "ws/webportal/params",
                   body = list(wkt = area_query, fq = taxa_query,
                               fields = columns))
  list(fq = filter_query, q = paste0("qid:", resp))
}

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

# convert true/false to logical values
fix_assertion_cols <- function(df, assertion_cols) {
  for (col in assertion_cols) {
    df[, col] <- as.logical(df[, col])
  }
  df
}

build_columns <- function(col_df) {
  if (nrow(col_df) == 0) {
    return("")
  }
  ala_cols <- dwc_to_ala(col_df$name)
  paste0(ala_cols, collapse = ",")
}

user_agent_string <- function() {
  version_string <- "version unknown"
  suppressWarnings(
    try(version_string <- utils::packageDescription("galah")[["Version"]],
        silent = TRUE)) ## get the galah version, if we can
  paste0("galah ", version_string)
}


