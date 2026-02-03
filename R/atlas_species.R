#' @rdname atlas_
#' @order 3
#' @export
atlas_species <- function(request = NULL,
                          identify = NULL,
                          filter = NULL,
                          geolocate = NULL,
                          apply_profile = NULL
                          ) {
                            
  # capture supplied arguments
  args <- as.list(environment())
  
  # handle type correctly
  if(!is.null(args$request)){
    args$request$type <- "species"
  }else{
    args <- c(list(type = "species"), args)
  }

  # convert to `data_request` object
  check_atlas_inputs(args) |>
    check_distinct_for_atlas_species() |>
    collapse() |>
    collect()
}

#' Internal micro-function to ensure that grouping is always applied, 
#' but can also be overruled by the user
#' @noRd
#' @keywords Internal
check_distinct_for_atlas_species <- function(.query){
  if(is.null(.query$distinct)){
    .query |>
      distinct("speciesID", .keep_all = TRUE)
  }else{
    .query$distinct$keep_all <- TRUE
    .query
  }
}