#' List valid download reasons
#'
#' When downloading occurrence data with [atlas_occurrences()] the
#' ALA APIs require a reason for download to be specified. By default, a
#' download reason of 'scientific research' is set for you, but if you wish to
#' change this you can do so with [galah_config()]. Use this function
#' to view the list of download reason code and names. When specifying a reason,
#' you can use either the download code or name.
#' @rdname show_all_reasons
#' @seealso This function is helpful in setting up [galah_config()].
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) of 
#' valid download reasons, containing the id and name for each reason.
#' @export
show_all_reasons <- function() {
  local_check <- galah_internal()$show_all_reasons
  if(!is.null(local_check)){
    local_check
  }else{
    ## return list of valid "reasons for use" codes
    out <- atlas_GET(server_config("logger_base_url"),
                   path = "service/logger/reasons")
    if (any(names(out) == "deprecated")) out <- out[!out$deprecated, ]
    out <- out[wanted_columns("reasons")]
    # sort by id to make it less confusing
    row.names(out) <- out$id
    df <- out[order(out$id),]
    galah_internal(show_all_reasons = df)
    return(df |> as_tibble())
  }
}