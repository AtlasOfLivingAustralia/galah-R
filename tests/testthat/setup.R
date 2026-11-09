# set up quiet versions of repeated functions

quiet_collapse <- function(x, ...){
  collapse_fun <- purrr::quietly(dplyr::collapse)
  collapse_fun(x, ...) |>
    purrr::pluck("result")
}
quiet_compute <- function(x){
  compute_fun <- purrr::quietly(dplyr::compute)
  compute_fun(x) |>
    purrr::pluck("result")
}
quiet_collect <- function(x, ...){
  purrr_collect <- purrr::quietly(dplyr::collect)
  purrr_collect(x, ...) |> 
    purrr::pluck("result")
}