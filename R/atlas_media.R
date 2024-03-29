#' Get metadata on images, sounds and videos
#'
#' In addition to text data describing individual occurrences and their 
#' attributes, ALA stores images, sounds and videos associated with a given 
#' record. `atlas_media` displays metadata for any and all of the media types. 
#'
#' @param request optional `data_request` object: generated by a call to
#' [galah_call()].
#' @param identify `data.frame`: generated by a call to
#' [galah_identify()].
#' @param filter `data.frame`: generated by a call to
#' [galah_filter()]
#' @param select `list`: generated by a call to [galah_select()]
#' @param geolocate `string`: generated by a call to
#' [galah_geolocate()]
#' @param data_profile `string`: generated by a call to
#' [galah_apply_profile()]
#' @details [atlas_media()] works by first finding all occurrence records
#' matching the filter which contain media, then downloading the metadata for the
#' media. To actually download the files themselves, use [collect_media()].
#' It may be beneficial when requesting a large number of records to show a progress
#' bar by setting `verbose = TRUE` in [galah_config()].
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' of metadata of the requested media.
#' @seealso [atlas_counts()] to find the number of records with media; but note this
#' is not necessarily the same as the number of media files, as each record can have
#' more than one media file associated with it (see examples section for how to do this).
#' 
#' @examples \dontrun{
#' # Download Regent Honeyeater records with multimedia attached
#' galah_call() |>
#'   galah_identify("Regent Honeyeater") |>
#'   galah_filter(year == 2011) |>
#'   atlas_media()
#' 
#' # Download multimedia
#' galah_call() |>
#'   galah_identify("Regent Honeyeater") |>
#'   galah_filter(year == 2011) |>
#'   atlas_media() |>
#'   collect_media(path = "folder/your-directory")
#' 
#' # Specify a single media type to download
#' galah_call() |>
#'   galah_identify("Eolophus Roseicapilla") |>
#'   galah_filter(multimedia == "Sound") |>
#'   atlas_media()
#' 
#' # It's good to check how many records have media files before downloading
#' galah_call() |>
#'   galah_filter(multimedia == c("Image", "Sound", "Video")) |>
#'   galah_group_by(multimedia) |>
#'   atlas_counts()
#'   
#'   
#' # post version 2.0, it is possible to run all steps in sequence
#' # first, get occurrences, making sure to include media fields:
#' occurrences_df <- request_data() |>
#'   identify("Regent Honeyeater") |>
#'   filter(!is.na(images), year == 2011) |>
#'   select(group = "media") |>
#'   collect()
#'  
#' # second, get media metadata
#' media_info <- request_metadata() |>
#'   filter(media == occurrences_df) |>
#'   collect()
#'   
#' # the two steps above + `right_join()` are synonmous with `atlas_media()`
#' # third, get images
#' request_files() |>
#'   filter(media == media_df) |>
#'   collect(thumbnail = TRUE)
#'   
#' # step three is synonymous with `collect_media()`
#'}
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr relocate
#' @importFrom dplyr right_join
#' @importFrom dplyr join_by
#' @importFrom glue glue
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_longer
#' @export
atlas_media <- function(request = NULL, 
                        identify = NULL, 
                        filter = NULL, 
                        select = NULL,
                        geolocate = NULL,
                        data_profile = NULL
                        ) {
  
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
      .query <- x
    }
  } # end `select` checks
 
  # `filter` to records that contain media of valid types
  media_fq <- glue("({present_fields}:*)")
  if(length(present_fields) > 1){
    media_fq <- glue("({glue_collapse(media_fq, ' OR ')})")  
  }
  
  # update .query with fields filter
  # note that behaviour here depends on whether we have run compute_checks() above
  if(inherits(.query, "data_request")){
    .query$filter <- bind_rows(.query$filter, 
                              tibble(variable = "media",
                                     logical = "==",
                                     value = paste(present_fields, collapse = "|"),
                                     query = as.character(media_fq)))  
  }else if(inherits(.query, "query")){ # i.e. if .query is already a `query`
    url <- url_parse(.query$url)
    url$query$fq <- paste0(url$query$fq, "AND", media_fq)
    .query$url <- url_build(url)
    .query <- compute_occurrences(.query)
  }else{
    abort("unknown object class in `atlas_media()`")
  }
  
  # get occurrences
  occ <- .query |> 
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
