#' @rdname galah_call
#' @param x an object of class `data_request`
#' @export
print.data_request <- function(x, ...){
  filled_slots <- !unlist(lapply(x, is.null))
  if(any(filled_slots)){
    cat("An object of type `data_request` containing:\n\n")
    print(as.list(x[filled_slots]))
  }else{
    cat("An empty object of type `data_request`")
  }
}
# NOTE: use of `x` arg here is for consistency with `print()`; do not change