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
    collect() |> 
    dplyr::relocate(x$request$group_by$name) # place in supplied order

  # build filters for our next round of queries
  # for 3 facets, we need to expand our crossed variables
  if(ncol(facets) > 3){

    filters <- facets |>
      select(!!!names(facets[1:2])) |>
      purrr::map(.f = \(a){a[!is.na(a)]}) |>
      expand.grid(stringsAsFactors = FALSE)
    facet <- colnames(facets)[3]

    z <- x$request
    z$slice_arrange <- NULL # this is a hack to avoid messy object update code
    filter_1 <- colnames(filters)[1]
    filter_2 <- colnames(filters)[2]

    body_list <- purrr::map(
      split(filters, seq_len(nrow(filters))), 
      \(a){
       value_1 <- a[[1]][[1]]
       value_2 <- a[[2]][[1]]
       result <- z |>
          filter({{filter_1}} == {{value_1}},
                 {{filter_2}} == {{value_2}}) |> 
          group_by(facet) |>
          collapse()
        result$body
      })


  # for 2 facets, we just select the levels we need
  }else{
    filters <- facets |>
      select(!!!(colnames(facets)[1])) |>
      tidyr::drop_na()
    facet <- colnames(facets)[2]
    # extract a query that we can update with new parameters
    z <- x$request
    z$slice_arrange <- NULL # this is a hack to avoid messy object update code
    filter_name <- colnames(filters)[1]
    # create multiple new queries from the old one
    # this works because new calls to `filter()` *adds* to a query
    # while new calls to `group_by()` *replaces* old entries
    body_list <- purrr::map(filters[[1]], \(a){
      result <- z |>
        filter({{filter_name}} == {{a}}) |> 
        group_by(facet) |>
        collapse()
      result$body
    })
  }
  
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