wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", "taxon_concept_id", "rank",
                      "match_type", "kingdom", "phylum", "class", "order",
                      "family", "genus", "species", "vernacular_name",
                      "issues"),
           "extended_taxa" = c("subkingdom", "superclass", "infraclass",
                               "subclass", "subinfraclass", "suborder",
                               "superorder", "infraorder", "infrafamily",
                               "superfamily", "subfamily","subtribe", "subgenus"),
           "profile" = c("id", "name", "shortName", "description"),
           "media" = c("rightsHolder", "license", "creator", "title", "rights",
                       "mimetype", "media_id"),
           "layer" = c("layer_id", "description", "link"),
           "fields" = c("id", "description"),
           "assertions" = c("id", "description"),
           "quality_filter" = c("description", "filter"),
           "reasons" = c("id", "name"))
}

# rename specific columns, and convert camelCase to snake_case
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

build_query <- function(taxa, filters, locations, columns = NULL,
                        profile = NULL) {
  query <- list()
  if (is.null(taxa)) {
    taxa_query <- NULL
  } else {
    check_taxa_arg(taxa)
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
    # remove profile from filter rows
    filters <- filters[filters$name != "profile",]
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

# takes a dataframe and returns a built filter query
build_filter_query <- function(filters) {
  mapply(query_term, filters$name, filters$value, filters$include,
         USE.NAMES = FALSE)
}

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
    value_str <- paste0("(", paste(paste0("-", name), value,
                                   collapse = ' AND ', sep = ":"), ")")
  }
  #paste0("(", value_str, ")")
  value_str
}


filter_value <- function(val) {
  # replace logical values with strings
  if (is.logical(val)) {
    return(ifelse(val, "true", "false"))
  }
  val
}

# this is only relevant for ala_counts and ala_occurrences
cached_query <- function(taxa_query, filter_query, area_query,
                         columns = NULL) {
  url <- server_config("records_base_url")
  resp <- ala_POST(url, path = "ws/webportal/params",
                   body = list(wkt = area_query, fq = taxa_query,
                               fields = columns))
  list(fq = filter_query, q = paste0("qid:", resp))
}


extract_profile <- function(filters) {
  profile <- NULL
  if (!is.null(filters)){
    profile_row <- filters[filters$name == "profile",]
    if (nrow(profile_row) == 1) { profile <- profile_row$value[[1]] }
  }
  profile
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
  paste0(col_df$name, collapse = ",")
}

user_agent_string <- function() {
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
      stop("`select_taxa` requires an identifier input generated by `select_taxa()` or a `taxize` function.")
    }
  }
}
