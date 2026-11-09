# setup script for galah test suite

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

# set up bespoke skip_ function to enforce preferred test behaviour

# check status and cache *before* running tests
is_online_realtime <- function(){!is.null(curl::nslookup("captive.apple.com", error = FALSE))}
if(is_online_realtime()){
  Sys.setenv("NOT_ONLINE" = "")
}else{
  Sys.setenv("NOT_ONLINE" = "true")
}
# note this differs from skip_if_offline() ONLY by caching the result
# this reduces pings to captive.apple.com

# encode this into a rule to pass to `skip`
is_online_cached <- function(){
  x <- Sys.getenv("NOT_ONLINE")
  if(x == ""){
    TRUE
  }else{ # including when == "true"
    FALSE
  }
}

# new function to enact desirable testing behaviour
skip_galah <- function(){
  skip_on_cran()
  skip_if(!is_online_cached(), "offline") 
  skip_on_ci()
}
