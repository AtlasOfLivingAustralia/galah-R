#' Internal function to run `capture()` for `type = "taxa"`, `method = "metadata`
#' @noRd
#' @keywords Internal
capture_taxa <- function(.query, call = rlang::caller_env()){
  if(is.null(.query$identify)){
    result <- list(type = "metadata/taxa",
                   atlas = .query$atlas)
  }else{
    if(ncol(.query$identify) > 1 | 
       colnames(.query$identify)[1] != "search_term"){
      result <- capture_taxa_multiple(.query, call = call)
    }else{
      result <- capture_taxa_single(.query, call = call)
    }
  }
  result |>
    as_query()
}

#' Internal function to `capture()` for a single taxonomic name 
#' @noRd
#' @keywords Internal
capture_taxa_single <- function(.query, call = rlang::caller_env()){
  .query$type <- "metadata/taxa-single"
  terms <- .query$identify$search_term
  
  # Match parsing of search term to ALA API formatting
  # Formats RFC 3986 reserved characters (eg. `(`, `)`, `+`)
  # Spaces are reformatted to "+"
  terms_formatted <- terms |>
    utils::URLencode(reserved = TRUE) |>
    gsub("%20", "+", x = _, fixed = TRUE)
  url <- url_lookup(.query,
                    name = terms_formatted,
                    error_call = call)
  
  # For GBIF (only), switch to CoL taxonomy
  if(.query$atlas == "Global"){
    url <- glue::glue("{url}&checklistKey=7ddf754f-d193-4cc9-b351-99906754a03b")
  }
  
  # build object and return
  list(type = .query$type,
       atlas = .query$atlas,
       url = tibble::tibble(url = url, 
                            search_term = terms),
       headers = build_headers())
}

#' Internal function to `collapse()` where multiple taxonomic levels are given 
#' @noRd
#' @keywords Internal
capture_taxa_multiple <- function(.query, call = rlang::caller_env()){

  .query$type <- "metadata/taxa-multiple"
  # get a data.frame, enforce use of accepted taxon levels
  identify_df <- .query$identify
  # match to known taxonomic levels
  col_names <- colnames(identify_df)
  col_names_lower <- tolower(col_names)
  currently_accepted_ranks <- accepted_ranks(.query)
  recognized_ranks <- col_names_lower %in% currently_accepted_ranks
  not_recognized_ranks <- !recognized_ranks

  # error if number of known ranks is zero
  if(!any(recognized_ranks)){
    c("No taxon ranks match those in the column names of the supplied tibble.", 
      i =  glue::glue("Unmatched columns: ",
        glue::glue_collapse(col_names[not_recognized_ranks], sep = ", ", last = " & ")), 
      i = glue::glue("Accepted ranks are: ",
        glue::glue_collapse(currently_accepted_ranks, sep = ", ", last = " & "))
    ) |>
    cli::cli_abort(call = call)
    
  # warn if some ranks are dropped
  }else if(any(not_recognized_ranks)){
    c("Some column names of the supplied tibble are not recognised taxonomic ranks.", 
      i =  glue::glue("Unmatched columns: ",
        glue::glue_collapse(col_names[not_recognized_ranks], sep = ", ", last = " & ")), 
      i = glue::glue("Retained columns: ",
        glue::glue_collapse(col_names[recognized_ranks], sep = ", ", last = " & "))
    ) |>
    cli::cli_warn(call = call)

    # use this to select
    colnames(identify_df) <- col_names_lower
    identify_df <- identify_df |>
      dplyr::select(tidyselect::any_of(currently_accepted_ranks))
  }

  # convert formatting across all columns, vectorized 
  # Match parsing of search term to ALA API formatting
    # Formats all RFC 3986 reserved characters (eg. `(`, `)`, `+`)
    # Spaces are reformatted to "+"
  identify_df <- purrr::map(as.list(identify_df),
             \(a){a |>
                         utils::URLencode(reserved = TRUE) |>
                         gsub("%20", "+", x = _, fixed = TRUE)})|>
    tibble::as_tibble()
  
  
  # split into one query per row
  split_list <- split(identify_df, 
                      seq_len(nrow(identify_df)))
  
  # create a set of valid urls
  base_url <- url_lookup(.query, error_call = call) |>
    httr2::url_parse()
  urls <- purrr::map(split_list,
                     function(a){
                       base_url$query <- a
                       httr2::url_build(base_url)
                     }) |>
    unlist()

  # For GBIF (only), switch to CoL taxonomy
  if(.query$atlas == "Global"){
    urls <- glue::glue("{urls}&checklistKey=7ddf754f-d193-4cc9-b351-99906754a03b")
  }

  # create a set of strings to show original search in final tibble
  search_terms <- purrr::map(split_list,
                             function(a){glue::glue_collapse(a, sep = "_")}) |>
    unlist()
  
  # build object and return
  list(type = .query$type,
       atlas = .query$atlas,
       url = tibble::tibble(url = urls, 
                            search_term = search_terms),
       headers = build_headers())
}

#' Internal function to create an identifiers query
#' @noRd
#' @keywords Internal
capture_identifiers <- function(.query, call = rlang::caller_env()){
  .query$type <- "metadata/identifiers"
  if(is.null(.query$filter)){
    url_list <- url_lookup(.query, error_call = call)
    names(url_list) <- "no-name-supplied"
  }else{
    search_terms <- .query$filter$value
    query <- as.list(search_terms)
    base_url <- url_lookup(.query, error_call = call)
    # create query urls
    if(grepl("api.gbif.org", base_url)){ # NOTE: This is not `.query$atlas == "GBIF"` because other atlases use this API
      base_url <- utils::URLdecode(base_url)
      urls <- glue::glue(base_url, id = query) |>
        unlist()
    }else{
      base_url <- httr2::url_parse(base_url)
      urls <- purrr::map(query,
                         function(a, base_url){
                           names(a) <- "taxonID"
                           base_url$query <- as.list(a)
                           httr2::url_build(base_url)
                         },
                         base_url = base_url) |>
        unlist()      
    }
  }
  # build object and return
  list(type = .query$type,
       atlas = .query$atlas,
       url = tibble::tibble(url = urls, 
                            search_term = search_terms),
       headers = build_headers()) |>
    as_query()
}

#' Internal function to accept only specific taxon ranks for searching
#' @noRd
#' @keywords Internal
accepted_ranks <- function(.query){
  if(.query$atlas == "Global"){
    # https://techdocs.gbif.org/en/openapi/v1/species#/Searching%20names/matchNames
    c("kingdom",
      "phylum",
      "class",
      "order",
      "superfamily",
      "family",
      "subfamily",
      "tribe",
      "subtribe",
      "genus",
      "species")
  }else{
    # https://docs.ala.org.au/openapi/index.html?urls.primaryName=namematching#/Taxonomy%20search/match_2
    c("kingdom",
      "phylum",
      "class",
      "order",
      "superfamily",
      "family",
      "genus",
      "specificEpithet",
      "infraspecificEpithet")
  }
}
