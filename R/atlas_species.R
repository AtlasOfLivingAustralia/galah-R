#' @rdname atlas_
#' @order 3
#' @importFrom potions pour
#' @export
atlas_species <- function(request = NULL,
                          identify = NULL,
                          filter = NULL,
                          geolocate = NULL,
                          data_profile = NULL
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
    collapse() |>
    collect()
}