#' @rdname atlas_
#' @order 4
#' @param all_fields `r lifecycle::badge("experimental")` If `TRUE`, 
#'   `show_values()` also returns all columns available from the media metadata
#'   API, rather than the 'default' columns traditionally provided via galah.
#' @export
atlas_media <- function(request = NULL, 
                        identify = NULL, 
                        filter = NULL, 
                        select = NULL,
                        geolocate = NULL,
                        apply_profile = NULL,
                        all_fields = FALSE
                        ) {
  
  # check media is available
  media_supported()

  # capture supplied arguments
  args <- as.list(environment())
  # convert to `data_request` object
  .query <- check_atlas_inputs(args)
  .query$type <- "occurrences" # default, but in case supplied otherwise
  
  # ensure a filter is present (somewhat redundant with `collapse`)
  if(is.null(.query$filter)){
    cli::cli_abort("You must specify a valid `filter()` to use `atlas_media()`")
  }

  # ensure media columns are present in `select`
  if(is.null(.query$select)){
    .query <- .query |>
      dplyr::select(group = c("basic", "media"))
    present_fields <- image_fields()
    present_fields <- present_fields[present_fields != "multimedia"] # check these fields for Spain
    query_collapse <- collapse(.query)
    # if `select` is present, ensure that at least one 'media' field is requested
  }else{
    x <- collapse(.query)
    
    # now check whether valid fields are present
    selected_fields <- x$url |>
      httr2::url_parse() |>
      purrr::pluck("query", "fields") |>
      strsplit(split = ",") |>
      purrr::pluck(!!!list(1))
    
    # `multimedia` should be in `select`, but not `filter`
    image_select <- image_fields()
    image_select <- image_select[image_select != "multimedia"] 
    
    # abort if no fields are given to `select`
    if(!any(selected_fields %in% image_select)){
      selected_text <- glue::glue_collapse(selected_fields, sep = ", ")
      c("No media fields requested by `select()`", 
        i = "try `galah_select({selected_text}, group = 'media')` instead") |>
        cli::cli_abort()
    }else{
      present_fields <- selected_fields[selected_fields %in% image_select]
      query_collapse <- x
    }
  } # end `select` checks
  
  # add media content to filters
  if(length(present_fields) > 0){
    # do region-specific filter parsing
    media_fq <- image_filters(present_fields)
    # add back to source object
    if(length(media_fq) > 1){
      media_fq <- glue::glue("({glue::glue_collapse(media_fq, ' OR ')})") 
    }
    url <- httr2::url_parse(query_collapse$url)
    url$query$fq <- glue::glue("{url$query$fq} AND {media_fq}")
    query_collapse$url <- httr2::url_build(url)
  }

  # get occurrences, expand to one row per media entry
  occ <- query_collapse |> 
    collect(wait = TRUE) |>
    build_media_id(media_fields = present_fields)

  # collect media metadata
  media_query <- request_metadata() |>
    filter(media == dplyr::pull(occ, "media_id"))
  if(isTRUE(all_fields)){
    media_query <- media_query |>
      dplyr::select(tidyselect::everything())
  }
  media <- collect(media_query)
  
  # join and return
  dplyr::right_join(occ,
                    media, 
                    by = dplyr::join_by("media_id"))
}

#' Internal function to get media metadata, and create a valid file name
#' @noRd
#' @keywords Internal
build_media_id <- function(df, media_fields){
  if(any(colnames(df) == "all_image_url")){
    df |>
      dplyr::mutate("media_id" = .data$all_image_url,
                    "media_type" = "images",
                  .before = 1) |>
      dplyr::select(-"all_image_url")
  }else{
    purrr::map(media_fields, .f = \(a){
     df |>
        tidyr::unnest_longer(col = a) |>
       dplyr::mutate(media_id = as.character(.data[[a]]),
                     media_type = as.character(a),
                    .before = 1) |>
       dplyr::filter(!is.na(.data$media_id)) |>
       dplyr::select(- tidyselect::any_of(media_fields))
    }) |>
    dplyr::bind_rows()
  }
}