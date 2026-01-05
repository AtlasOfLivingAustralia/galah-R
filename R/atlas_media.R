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
    media_fq <- parse_regional_media_filters(present_fields)
    # add back to source object
    if(length(media_fq) > 1){
      media_fq <- glue::glue("({glue::glue_collapse(media_fq, ' OR ')})") 
    }
    url <- httr2::url_parse(query_collapse$url)
    url$query$fq <- glue::glue("{url$query$fq} AND {media_fq}")
    query_collapse$url <- httr2::url_build(url)
  }

  # get occurrences
  occ <- query_collapse |> 
    collect(wait = TRUE) |>
    tidyr::unnest_longer(col = tidyselect::any_of(present_fields))
  
  if(!any(colnames(occ) == "all_image_url")){
    occ$media_id <- build_media_id(occ)
  }
  
  # collect media metadata
  media_query <- request_metadata() |>
    filter(media == dplyr::pull(occ, "media_id"))
  if(isTRUE(all_fields)){
    media_query <- media_query |>
      dplyr::select(tidyselect::everything())
  }
  media <- collect(media_query)
  
  # join and return
  if(any(colnames(occ) == "all_image_url")){
    occ <- dplyr::rename(occ, "media_id" = "all_image_url")
  }
  occ_media <- dplyr::right_join(occ,
                                 media, 
                                 by = dplyr::join_by("media_id"))
  dplyr::relocate(occ_media, "media_id", 1)
}

#' Set filters that work for media in each atlas
#' @noRd
#' @keywords Internal
parse_regional_media_filters <- function(present_fields,
                                         error_call = rlang::caller_env()){
  
  atlas <- potions::pour("atlas", "region")
  switch(atlas,
         "Austria" = "(all_image_url:*)",
         "Australia" = glue::glue("({present_fields}:*)"),
         "Brazil" = "(all_image_url:*)",
         # Flanders?
         "Guatemala" = "(all_image_url:*)",
         "Kew" =  "(all_image_url:*)",
         "Portugal" = "(all_image_url:*)",
         "Spain" = {filter_fields <- present_fields |>
                       stringr::str_remove("s$") |>
                       paste0("IDsCount")
                     glue::glue("{filter_fields}:[1 TO *]")},
         "Sweden" = {filter_fields <- present_fields |>
                       stringr::str_remove("s$") |>
                       paste0("IDsCount")
                     glue::glue("{filter_fields}:[1 TO *]")},
         "United Kingdom" = "(all_image_url:*)", # !is.na(all_image_url),
         cli::cli_abort("`atlas_media` is not supported for atlas = {atlas}",
                        call = error_call)
  )
}

#' Internal function to get media metadata, and create a valid file name
#' @noRd
#' @keywords Internal
build_media_id <- function(df){
  # create a column that includes media identifiers, regardless of which column they are in
  ## NOTE: I haven't found good tidyverse syntax for this yet
  x <- rep(NA, nrow(df))
  if(any(colnames(df) == "videos")){
    videos <- !is.na(df$videos)
    if(any(videos)){x[videos] <- df$videos[videos]}    
  }
  if(any(colnames(df) == "sounds")){
    sounds <- !is.na(df$sounds)
    if(any(sounds)){x[sounds] <- df$sounds[sounds]}
  }
  if(any(colnames(df) == "images")){
    images <- !is.na(df$images)
    if(any(images)){x[images] <- df$images[images]}
  }
  x
}