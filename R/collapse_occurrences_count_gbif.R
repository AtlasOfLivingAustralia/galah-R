#' Function to construct `body` arg for GBIF
#' predicates are JSON scripts for passing to GBIF offline downloads API.
#' https://www.gbif.org/developer/occurrence
#' @param x A list with slots relevant to building predicates
#' @noRd
#' @keywords Internal
collapse_occurrences_count_gbif <- function(x){
  x$body <- list(predicate = build_predicates(x$body),
                 limit = 0) |>
    remove_nulls_from_list() |>
    jsonlite::toJSON(auto_unbox = TRUE,
                     pretty = TRUE)
  x
}

#' Internal function to handle count queries with 'basic' group_by queries
#' In practice, this means just passing arguments to the `facets` arg
#' @noRd
#' @keywords Internal
collapse_occurrences_count_gbif_groupby_basic <- function(x){
  x$body <- list(predicate = build_predicates(x$body),
                 limit = 0,
                 facets = parse_predicates_groupby(x$body$group_by)) |>
    remove_nulls_from_list() |>
    jsonlite::toJSON(auto_unbox = TRUE,
                     pretty = TRUE)
  x
}

#' Internal function to handle 'crossed' count queries
#' @noRd
#' @keywords Internal
collapse_occurrences_count_gbif_groupby_crossed <- function(x){

  # get a 'basic' query, showing facets for each variable separately 
  facets <- collapse_occurrences_count_gbif_groupby_basic(x) |>
    collect()
  
  # then order queries by decreasing number of levels
  facet_order <- facets |> 
    select(-"count") |>
    purrr::map(.f = \(a){length(which(!is.na(a)))}) |> 
    unlist() |>
    sort()

  # build filters for our next round of queries
  # for 3 facets, we need to expand our crossed variables
  if(length(facet_order) > 2){
    filters <- facets |>
      select(!!!names(facet_order[1:2])) |>
      purrr::map(.f = \(a){a[!is.na(a)]}) |>
      expand.grid()
    facet <- names(facet_order)[3]
  # for 2 facets, we just select the levels we need
  }else{
    variable <- names(facet_order)[1]
    filters <- facets |>
      select(!!!(names(facet_order)[1])) |>
      tidyr::drop_na()
    facet <- names(facet_order)[2]
  }
  
  # convert our tibble of new filters into predicate entries
  predicate_list <- tibble_to_predicate(filters)
  
  # create separate `body` (JSON) files for each query, accounting for `filter`
  # and `facet`
  body_list <- purrr::map(predicate_list, .f = \(a){
    temp_obj <- x$body
    temp_obj$filter <- c(temp_obj$filter, a)
    temp_obj$group_by <- NULL
    list(predicate = build_predicates(temp_obj),
         limit = 0,
         facets = tibble::tibble(name = facet) |> 
           parse_predicates_groupby()) |>
      remove_nulls_from_list() |>
      jsonlite::toJSON(auto_unbox = TRUE,
                       pretty = TRUE)
  })
  
  # add predicates to tibble; tibble to `body`; return
  filters$predicate <- body_list
  x$body <- filters
  x
}

#' Internal function to convert a facet df into a predicate
#' @noRd
#' @keywords Internal
tibble_to_predicate <- function(df){
  # iterate over rows
  purrr::map(
    split(df, seq_len(nrow(df))),
    .f = \(a){
      # for each row, convert each column (cell) into a list
      cols_vector <- seq_len(ncol(a))
      purrr::map(cols_vector, \(b){
        list(type = "equals",
             key = gbif_upper_case(names(a)[b]),
             value = a[[b]][[1]])
      })
    })
}