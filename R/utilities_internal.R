wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", "taxon_concept_id", "rank",
                      "match_type", "kingdom", "phylum", "class", "order",
                      "family", "genus", "species", "issues"),
           "profile" = c("id", "name", "shortName", "description"),
           "media" = c("rightsHolder", "imageIdentifier", "format",
                       "occurrenceID", "recognisedLicence", "license",
                       "creator", "title", "rights", "mimeType",
                       "media_id"),
           "layer" = c("layer_id", "name", "source_link", "description"),
           "fields" = c("name", "data_type", "info", "class"),
           "assertions" = c("name", "data_type", "info", "class"),
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
    } else if (type == "fields") {
      varnames[varnames == "classs"] <- "class"
      varnames[varnames == "dataType"] <- "data_type"
    } else if (type == "assertions") {
      varnames[varnames == "description"] <- "info"
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

build_taxa_query <- function(ids) {
    # order filters so cached file can be found
    ids <- ids[order(ids)]
    paste0("(lsid:", paste(ids, collapse = " OR lsid:"), ")")
}

# this is only relevant for ala_counts and ala_occurrences
cached_query <- function(taxa_query, filter_query, area_query,
                         columns = NULL) {
  url <- getOption("koala_server_config")$base_url_biocache
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
    try(version_string <- utils::packageDescription("koala")[["Version"]],
        silent = TRUE)) ## get the koala version, if we can
  paste0("koala ", version_string)
}


