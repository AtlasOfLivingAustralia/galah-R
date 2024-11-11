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
  if(!(atlas %in% c("Austraila", "Sweden"))){
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
  media_fields <- c("images", "videos", "sounds")
  if(is.null(.query$select)){
    .query <- update_data_request(.query, 
                                  select = galah_select(group = c("basic", "media")))
    present_fields <- media_fields
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
    
    # abort if none are given
    if(!any(selected_fields %in% media_fields)){
      selected_text <- paste(selected_fields, collapse = ", ")
      bullets <- c("No media fields requested by `select()`", 
                   i = glue("try `galah_select({selected_text}, group = 'media')` instead"))
      abort(bullets)
    }else{
      present_fields <- selected_fields[selected_fields %in% media_fields]
      query_collapse <- x
    }
  } # end `select` checks
  
  # add media content to filters
  if(length(present_fields) > 0){
    # do region-specific filter parsing
    atlas <- pour("atlas", "region")
    if(atlas == "Sweden"){
      sbdi_filter_fields <- present_fields |>
        str_remove("s$") |>
        paste0("IDsCount")
      media_fq <- glue("{sbdi_filter_fields}:[1 TO *]")
    }else{ # i.e. Australia
      media_fq <- glue("({present_fields}:*)")
    }
    # add back to source object
    media_fq <- glue("({glue_collapse(media_fq, ' OR ')})")
    url <- url_parse(query_collapse$url)
    url$query$fq <- paste0(url$query$fq, " AND ", media_fq)
    query_collapse$url <- url_build(url)
  }

  # get occurrences
  occ <- query_collapse |> 
    collect(wait = TRUE) |>
    unnest_longer(col = any_of(present_fields))
  occ$media_id <- build_media_id(occ) 
  # collect media metadata
  media <- request_metadata() |>
    filter(media == occ) |>
    collect()
  # join and return
  occ_media <- right_join(occ, 
                          media, 
                          by = join_by("media_id" == "image_id"))
  relocate(occ_media, "media_id", 1)
}