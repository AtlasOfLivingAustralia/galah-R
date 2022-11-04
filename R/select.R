#' Select for object of class `data_request`
#' @description `r lifecycle::badge("experimental")` 
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @exportS3Method dplyr::select
#' @export
select.data_request <- function(.data, ..., group = c("basic", "event", "media", "assertions")
){
  dots <- enquos(..., .ignore_empty = "all")
  
  # If no args are supplied, set default columns returned as group = "basic"  
  if(missing(group) & length(dots) < 1){
    group <- "basic"
  }
  
  # Build a data.frame with a standardised set of names,
  # stored by galah_config()
  field_names <- unique(c(show_all_fields()$id, show_all_assertions()$id))
  df <- matrix(data = NA, nrow = 0, ncol = length(field_names),
               dimnames = list(NULL, field_names)) |>
    as.data.frame()
  
  # Match 'groups' of columns
  if (!missing(group) && !is.null(group)) {
    append_groups <- TRUE
    group <- match.arg(group, several.ok = TRUE)
    group_cols <- unlist(lapply(group, preset_cols))
    select_groups <- eval_select(all_of(group_cols), data = df) |> names()
  } else {
    append_groups <- FALSE
    select_groups <- NULL
  }
  
  # select
  if(length(dots) > 0){
    select_individuals <- unlist(lapply(dots, function(a){
      tidyselect::eval_select(a, data = df) |> names()
    }))
  }else{
    select_individuals <- NULL
  }
  
  all_cols <- tibble(name = unique(c(select_groups, select_individuals)))
  all_cols$type <- ifelse(str_detect(all_cols$name, "[[:lower:]]"), 
                          "field", 
                          "assertions")
  attr(all_cols, "call") <- "galah_select" 
  if(append_groups){
    attr(all_cols, "groups") <- group 
  }
  
  update_galah_call(.data, select = all_cols)
}