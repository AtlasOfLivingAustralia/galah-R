#' @rdname atlas_
#' @order 4
#' @importFrom dplyr any_of
#' @importFrom dplyr relocate
#' @importFrom dplyr right_join
#' @importFrom dplyr join_by
#' @importFrom glue glue
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @importFrom potions pour
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom stringr str_remove
#' @importFrom tidyr unnest_longer
#' @export
atlas_media <- function(request = NULL, 
                        identify = NULL, 
                        filter = NULL, 
                        select = NULL,
                        geolocate = NULL,
                        data_profile = NULL
                        ) {
  
  atlas <- pour("atlas", "region", .pkg = "galah")
  supported_atlases <- c("Australia",
                         "Austria", # not currently working
                         "Brazil",
                         "Guatemala",
                         "Sweden", 
                         "Spain", 
                         "United Kingdom")
  if(!(atlas %in% supported_atlases)){
    abort(glue("`atlas_media` is not supported for atlas = {atlas}"))
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
      url_parse() |>
      pluck("query", "fields") |>
      strsplit(split = ",") |>
      pluck(!!!list(1))
    
    # `multimedia` should be in `select`, but not `filter`
    image_select <- image_fields()
    image_select <- image_select[image_select != "multimedia"] 
    
    # abort if no fields are given to `select`
    if(!any(selected_fields %in% image_select)){
      selected_text <- paste(selected_fields, collapse = ", ")
      bullets <- c("No media fields requested by `select()`", 
                   i = glue("try `galah_select({selected_text}, group = 'media')` instead"))
      abort(bullets)
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
      media_fq <- glue("({glue_collapse(media_fq, ' OR ')})") 
    }
    url <- url_parse(query_collapse$url)
    url$query$fq <- paste0(url$query$fq, " AND ", media_fq)
    query_collapse$url <- url_build(url)
  }

  # get occurrences
  occ <- query_collapse |> 
    collect(wait = TRUE) |>
    unnest_longer(col = any_of(present_fields))
  
  if(!any(colnames(occ) == "all_image_url")){
    occ$media_id <- build_media_id(occ)
  }

  # collect media metadata
  media <- request_metadata() |>
    filter(media == occ) |>
    collect()
  
  # join and return
  if(any(colnames(occ) == "all_image_url")){
    occ <- rename(occ, "media_id" = "all_image_url")
  }
  occ_media <- right_join(occ,
                          media, 
                          by = join_by("media_id" == "image_id"))
  relocate(occ_media, "media_id", 1)
}

#' Set filters that work for media in each atlas
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @noRd
#' @keywords Internal
parse_regional_media_filters <- function(present_fields){
  
  atlas <- pour("atlas", "region")
  switch(atlas,
         "Austria" = "(all_image_url:*)",
         "Australia" = glue("({present_fields}:*)"),
         "Brazil" = "(all_image_url:*)",
         "Guatemala" = "(all_image_url:*)",
         "Portugal" = "(all_image_url:*)",
         "Spain" = {filter_fields <- present_fields |>
                       str_remove("s$") |>
                       paste0("IDsCount")
                     glue("{filter_fields}:[1 TO *]")},
         "Sweden" = {filter_fields <- present_fields |>
                       str_remove("s$") |>
                       paste0("IDsCount")
                     glue("{filter_fields}:[1 TO *]")},
         "United Kingdom" = "(all_image_url:*)", # !is.na(all_image_url),
         abort(glue("`atlas_media` is not supported for atlas = {atlas}"))
  )
}