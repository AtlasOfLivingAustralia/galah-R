#' Keep distinct/unique rows
#' 
#' Keep only unique/distinct rows from a data frame. This is similar to 
#' [unique.data.frame()] but considerably faster. It is evaluated lazily.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a 
#' lazy data frame (e.g. from dbplyr or dtplyr). See Methods, below, 
#' for more details.
#' @param ... Variables to use when determining uniqueness. Unlike the `dplyr`
#' implementation this must be set for the function to do anything, and only 
#' a single variable is used.
#' @param .keep_all If `TRUE`, keep all variables in .data. Defaults to
#' `FALSE`
#' @details
#' This function has several potential uses. In it's default mode, it simply 
#' shows the unique values for a supplied field:
#' 
#' \preformatted{galah_call() |>
#'   distinct(basisOfRecord) |> 
#'   collect()
#' 
#' # A tibble: 9 × 1
#'   basisOfRecord      
#'   <chr>              
#' 1 HUMAN_OBSERVATION  
#' 2 PRESERVED_SPECIMEN 
#' 3 OCCURRENCE         
#' 4 MACHINE_OBSERVATION
#' 5 OBSERVATION        
#' 6 MATERIAL_SAMPLE    
#' 7 LIVING_SPECIMEN    
#' 8 FOSSIL_SPECIMEN    
#' 9 MATERIAL_CITATION
#' }
#' 
#' This is the same result as you would get using [show_values()]:
#' 
#' \preformatted{search_all(fields, "basisOfRecord") |> 
#'   show_values()}
#' 
#' Using [distinct()] is somewhat more reliable, however, as it doesn't rely
#' on searching the tibble returned by `show_all(fields)`. It is also more
#' efficient, particularly when caching is turned off. If the goal is to 
#' retrieve the _number_ of levels of a factor, use:
#' 
#' \preformatted{galah_call() |>
#'   distinct(basisOfRecord) |> 
#'   count() |>
#'   collect()
#' 
#' # A tibble: 1 × 1
#'   count
#'   <int>
#' 1     9
#' }
#' 
#' When the variable passed to [distinct()] in the above example is 
#' `speciesID`, this is identical to calling:
#' 
#' \preformatted{atlas_counts(type = "species")}
#' 
#' You can also pass \code{\link[=group_by.data_request]{group_by()}} 
#' to find the number of facets per level of a second variable:
#' 
#' \preformatted{galah_call() |>
#'   identify("Perameles") |>
#'   distinct(speciesID) |> 
#'   group_by(basisOfRecord) |>
#'   count() |>
#'   collect()
#' 
#' # A tibble: 8 × 2
#'   basisOfRecord       count
#'   <chr>               <int>
#' 1 Human observation       7
#' 2 Preserved specimen      9
#' 3 Machine observation     2
#' 4 Observation             3
#' 5 Occurrence              3
#' 6 Material Sample         4
#' 7 Fossil specimen         1
#' 8 Living specimen         1
#' }
#' 
#' By setting `.keep_all = TRUE`, we get more information on each record.
#' Due to limits on the APIs this is not a perfect analogy for running 
#' [dplyr::distinct()] on raw occurrences; but it does allow us to
#' generalise [atlas_species()] to use any taxonomic identifier. For example,
#' we might choose to show data by family instead of species:
#' 
#' \preformatted{galah_call() |>
#'   identify("Coleoptera") |>
#'   distinct(familyID, .keep_all = TRUE) |> 
#'   collect()}
#' 
#' Using [group_by()] is also valid:
#' 
#' \preformatted{galah_call() |>
#'     filter(year == 2024,
#'            genus == "Crinia") |>
#'     group_by(speciesID) |>
#'     distinct(.keep_all = TRUE) |>
#'     collapse()}
#' 
#' In this case, \code{\link[=collect.data_request]{collect()}} and 
#' [atlas_species()] are synonymous, with the exception that the latter
#' does not require you to set the `.keep_all` argument to `TRUE`. So you 
#' could instead use:
#' 
#' \preformatted{galah_call() |>
#'   identify("Coleoptera") |>
#'   distinct(familyID) |> 
#'   atlas_species()}
#'
#' @examples \dontrun{
#' galah_call() |>
#'   distinct(basisOfRecord) |>
#'   count() |>
#'   collect()
#' }
#' @export
distinct.data_request <- function(.data, 
                                  ...,
                                  .keep_all = FALSE){
  # NOTE: internally this is based on `group_by.data_request`
  # BUT there are cases where we need to distinguish between `group_by()` and `distinct()`,
  # hence the separate slots
  parsed_dots <- rlang::enquos(...,
                               .ignore_empty = "all") |>
    parse_quosures_basic() |>
    parse_distinct(keep_all = .keep_all)

  update_request_object(.data,
                        distinct = parsed_dots)
}

#' Internal parsing of `distinct` args
#' @noRd
#' @keywords Internal
parse_distinct <- function(dot_names, 
                           keep_all = FALSE,
                           error_call = rlang::caller_env()){
  if(length(dot_names) > 0){
    if(length(dot_names) > 1){
      c(
        "Too many fields supplied.",
        i = "`distinct.data_request` only accepts one field.") |>
        cli::cli_abort(call = error_call)
    }
    if(length(dot_names) > 0){
      names(dot_names) <- NULL # needed to avoid empty strings added as names
      tibble::tibble(name = dot_names,
                     keep_all = keep_all) 
    }else{
      tibble::tibble(name = NA,
                     keep_all = keep_all)
    }
  }else{
    tibble::tibble(name = NA,
                   keep_all = keep_all)
  }
}