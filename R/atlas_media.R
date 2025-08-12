#' @rdname atlas_
#' @order 4
#' @export
atlas_media <- function(request = NULL, 
                        identify = NULL, 
                        filter = NULL, 
                        select = NULL,
                        geolocate = NULL,
                        data_profile = NULL
                        ) {
  
  atlas <- potions::pour("atlas", "region",
                         .pkg = "galah")
  supported_atlases <- c("Australia",
                         "Austria", # not currently working
                         "Brazil",
                         "Guatemala",
                         "Kew",
                         "Sweden",
                         "Spain",
                         "United Kingdom")
  if(!(atlas %in% supported_atlases)){
    cli::cli_abort("`atlas_media` is not supported for atlas = {atlas}")
  }

  # capture supplied arguments
  args <- as.list(environment())
  # convert to `data_request` object
  .query <- check_atlas_inputs(args)
  .query$type <- "occurrences" # default, but in case supplied otherwise
  
  # ensure a filter is present (somewhat redundant with `collapse`)
  if(is.null(.query$filter)){
    abort("You must specify a valid `filter()` to use `atlas_media()`")
  }

  # ensure media columns are present in `select`
  if(is.null(.query$select)){
    .query <- update_data_request(.query, 
                                  select = galah_select(group = c("basic", "media")))
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
      media_fq <- glue::glue("({glue_collapse(media_fq, ' OR ')})") 
    }
    url <- httr2::url_parse(query_collapse$url)
    url$query$fq <- glue::glue("{url$query$fq} AND {media_fq}")
    query_collapse$url <- httr2::url_build(url)
  }

  # get occurrences
  occ <- query_collapse |> 
    collect(wait = TRUE) |>
    tidyr::unnest_longer(col = any_of(present_fields))
  
  if(!any(colnames(occ) == "all_image_url")){
    occ$media_id <- build_media_id(occ)
  }

  # collect media metadata
  media <- request_metadata() |>
    filter(media == occ) |>
    collect()
  
  # join and return
  if(any(colnames(occ) == "all_image_url")){
    occ <- dplyr::rename(occ, "media_id" = "all_image_url")
  }
  occ_media <- dplyr::right_join(occ,
                                 media, 
                                 by = dplyr::join_by("media_id" == "image_id"))
  dplyr::relocate(occ_media, "media_id", 1)
}

#' Set filters that work for media in each atlas
#' @noRd
#' @keywords Internal
parse_regional_media_filters <- function(present_fields){
  
  atlas <- potions::pour("atlas", "region")
  switch(atlas,
         "Austria" = "(all_image_url:*)",
         "Australia" = glue("({present_fields}:*)"),
         "Brazil" = "(all_image_url:*)",
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
         cli::cli_abort("`atlas_media` is not supported for atlas = {atlas}")
  )
}