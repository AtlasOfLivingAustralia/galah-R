#' Internal function to convert `data_request` with `type = "species"` to a `query`
#' @noRd
#' @keywords Internal
as_query_species <- function(.query){
  if(is_gbif()){
    result <- as_query_occurrences_gbif(.query, 
                                        format = "SPECIES_LIST")
    result$type <- "data/species"
    result
  }else{
    as_query_species_atlas(.query)
  }
}

#' calculate the query to be returned for a given living atlas
#' @noRd
#' @keywords Internal
as_query_species_atlas <- function(.query){
  # set default columns
  if(is.null(.query$select)){
    .query$select <- galah_select(group = "taxonomy")
  }
  
  # determine whether to use `group_by` or `species_facets()`
  if(is.null(.query$group_by)){
    .query$group_by <- tibble::tibble(name = species_facets(),
                                      type = "field")
  }
  
  # build a query
  query <- c(
    build_query(.query$identify, 
                .query$filter, 
                .query$geolocate, 
                .query$data_profile),
    emailNotify = email_notify(),
    sourceTypeId = 2004,
    reasonTypeId = pour("user", "download_reason_id"),
    email = potions::pour("user", "email"), 
    facets = .query$group_by$name,
    parse_select_species(.query$select)
  )
  # build url
  url <- url_lookup("data/species") |> 
    httr2::url_parse()
  url$query <- query
  # build output
  result <- list(
    type = "data/species",
    url = httr2::url_build(url),
    headers = build_headers(),
    filter = .query$filter,
    group_by = .query$group_by,
    download = TRUE)
  class(result) <- "query"
  result
}

#' parse `select()` for `atlas_species()`
#' @noRd
#' @keywords Internal
parse_select_species <- function(.select){
  # parse labels for supplied field names
  quosure_check <- purrr::map(.select, is_quosure) |> 
    unlist()
  if(any(quosure_check)){
    named_fields <- purrr::map(.select[quosure_check], as_label) |> 
      unlist()
  }else{
    named_fields <- NULL
  }
  # create output
  result <- list(
    lookup = "false",
    count = "false",
    synonym = "false",
    lists = "false")
  # fill list with correct results
  if(!is.null(.select$group)){
    if(any(.select$group == "taxonomy")){
      result$lookup <- "true"
    }
  }
  if(!is.null(named_fields)){
    # check for unexpected names
    name_check <- !(named_fields %in% c("counts", "synonyms", "lists"))
    if(any(name_check)){
      unexpected_names <- glue::glue_collapse(named_fields[name_check], last = " and ")
      c("When type = 'species', `select()` only accepts 'counts', 'synonyms' or 'lists' as valid fields.",
        i = glue("Unexpected fields: {unexpected_names}")) |>
      cli::cli_warn()
    }
    # parse 'correct' names
    if(any(named_fields == "counts")){result$count <- "true"}
    if(any(named_fields == "synonyms")){result$synonym <- "true"}
    if(any(named_fields == "lists")){result$lists <- "true"}
  }
  result
}
