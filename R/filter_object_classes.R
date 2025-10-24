#' Object classes for `filter()` queries
#' 
#' In galah, there are several ways to provide filter information. To ensure
#' these are handled and printed correctly, they are assigned classes
#' @param x a list
#' @rdname filter_object_classes
#' @order 1
#' @export
as_data_filter <- function(x){
  structure(x, class = c("data_filter",
                         "tbl_df",
                         "tbl",
                         "data.frame"))
}

#' @rdname filter_object_classes
#' @order 2
#' @export
as_predicates_filter <- function(x){
  x |>
    structure(class = c("predicates_filter", 
                        "list"))
}

#' @rdname filter_object_classes
#' @order 3
#' @export
as_metadata_filter <- function(x){
  x |>
    structure(class = c("metadata_filter", 
                        "list"))
}

#' @rdname filter_object_classes
#' @order 4
#' @export
as_files_filter <- function(x){
  x |>
    structure(class = c("files_filter",
                        "list"))
}


# Print functions for the above

#' @rdname filter_object_classes
#' @order 5
#' @export
print.data_filter <- function(x, ...){
 filter_string <- basic_filter_print(x) 
 glue::glue("Object of class `data_filter`: {filter_string}") |>
   cat()
}

#' @rdname filter_object_classes
#' @order 6
#' @export
print.predicates_filter <- function(x, ...){
  # object of class `predicates_filter`
  predicates_string <- glue::glue_collapse(unlist(x), sep = " ") 
  glue::glue("Object of class `predictes_filter`: {predicates_string}") |>
    cat()
}

#' @rdname filter_object_classes
#' @order 7
#' @export
print.metadata_filter <- function(x, ...){
  glue::glue("Object of class `metadata_filter` with type `{x$variable}` (n = {length(x$data)} entries)") |>
    cat()
}

#' @rdname filter_object_classes
#' @order 8
#' @export
print.files_filter <- function(x, ...){
  glue::glue("Object of class `files_filter` with {nrow(x$data)} rows") |>
    cat()
}

#' Internal function to print filter statements
#' @noRd
#' @keywords Internal
basic_filter_print <- function(x){
  if(ncol(x) > 2){
    df <- x[, 1:3]
  }else{
    df <- x
  }
  if(nrow(df) > 1){
    df <- df[1, ]  
  }
  glue::glue_collapse(
    apply(df, 1, function(b){paste(b, collapse = " ")}),
    sep = " | ")
}