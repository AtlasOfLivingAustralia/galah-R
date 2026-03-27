#' Internal function to run `capture()` for `type = "taxa"`, `method = "metadata`
#' @noRd
#' @keywords Internal
capture_taxa <- function(.query){
  if(is.null(.query$identify)){
    result <- list(type = "metadata/taxa",
                   atlas = .query$atlas)
  }else{
    if(ncol(.query$identify) > 1 | 
       colnames(.query$identify)[1] != "search_term"){
      result <- capture_taxa_multiple(.query)
    }else{
      result <- capture_taxa_single(.query)
    }
  }
  result |>
    as_query()
}

#' Internal function to `capture()` for a single taxonomic name 
#' @noRd
#' @keywords Internal
capture_taxa_single <- function(.query){
  .query$type <- "metadata/taxa-single"
  terms <- .query$identify$search_term
  list(type = .query$type,
       atlas = .query$atlas,
       url = tibble::tibble(url = url_lookup(.query, name = terms), 
                            search_term = terms),
       headers = build_headers())
}

#' Internal function to `collapse()` where multiple taxonomic levels are given 
#' @noRd
#' @keywords Internal
capture_taxa_multiple <- function(.query){
  .query$type <- "metadata/taxa-multiple"
  # get a data.frame, enforce use of accepted taxon levels
  identify_df <- .query$identify
  colnames(identify_df) <- colnames(identify_df) |>
    tolower()
  identify_df <- identify_df |>
    dplyr::select(dplyr::any_of(accepted_ranks(.query)))
  
  # split into one query per row
  split_list <- split(.query$identify, 
                      seq_len(nrow(.query$identify)))
  base_url <- url_lookup(.query) |>
    httr2::url_parse()
  urls <- purrr::map(split_list,
                     function(a, base_url){
                       base_url$query <- as.list(a)
                       httr2::url_build(base_url)
                     },
                     base_url = base_url) |>
    unlist()
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
capture_identifiers <- function(.query){
  .query$type <- "metadata/identifiers"
  if(is.null(.query$filter)){
    url_list <- url_lookup(.query)
    names(url_list) <- "no-name-supplied"
  }else{
    search_terms <- .query$filter$value
    query <- as.list(search_terms)
    base_url <- url_lookup(.query)
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
