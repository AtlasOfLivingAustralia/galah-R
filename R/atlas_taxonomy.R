#' @rdname atlas_
#' @order 5
#' @param constrain_ids `string`: Optional string to limit which `taxon_concept_id`'s
#' are returned. This is useful for restricting taxonomy to particular 
#' authoritative sources. Default is `"biodiversity.org.au"` for Australia, 
#' which is the infix common to National Species List IDs; use
#' `NULL` to suppress source filtering. Regular expressions are supported.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom potions pour
#' @importFrom purrr list_flatten
#' @importFrom purrr pluck_depth
#' @importFrom stringr str_to_title
#' @keywords internal
#' @export
atlas_taxonomy <- function(request = NULL,
                           identify = NULL, 
                           filter = NULL,
                           constrain_ids = NULL
                           ) {
  # capture supplied arguments
  args <- as.list(environment())

  # convert to a valid `data_request` object
  .query <- check_atlas_inputs(args)
  .query$type <- "taxonomy" # default, but in case supplied otherwise
  check_identify(.query)
  check_down_to(.query)
  constrain_ids <- check_constraints(args = args, 
                                     call = as.list(sys.call()))

  # extract required information from `identify` 
  taxa_info <- request_metadata(type = "taxa") |>
    identify(.query$identify$search_term) |>
    collect()
  start_row <- taxa_info |>
    mutate(name = str_to_title(taxa_info$scientific_name),
           parent_taxon_concept_id = NA) |>
    select("name", 
           "rank", 
           "taxon_concept_id", 
           "parent_taxon_concept_id") 

  # build then flatten a tree
  taxonomy_tree <- drill_down_taxonomy(start_row, 
                                       down_to = .query$filter$value,
                                       constrain_ids = constrain_ids)
  for(i in seq_len(pluck_depth(taxonomy_tree))){
    taxonomy_tree <- list_flatten(taxonomy_tree)
  }
  result <- bind_rows(taxonomy_tree)
  
  # remove rows with ranks that are too low
  index <- rank_index(result$rank)
  down_to_index <- rank_index(.query$filter$value)
  result |>
    filter({{index}} <= {{down_to_index}} | is.na({{index}})) |>
    select("name", 
           "rank", 
           "parent_taxon_concept_id", 
           "taxon_concept_id")
}

#' Internal function to check whether constraints have been passed
#' @importFrom potions pour
#' @noRd
#' @keywords Internal
check_constraints <- function(args, call){
  if(any(names(call) == "constrain_ids")){ # i.e. if user specifies an argument
    call$constrain_ids
  }else{ # if NULL only occurs because no argument is set
    if(pour("atlas", "region") == "Australia"){
      "biodiversity.org.au"
    }
  }
} 

#' Internal recursive function to get child taxa
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @noRd
#' @keywords Internal
drill_down_taxonomy <- function(df, 
                                down_to,
                                constrain_ids = NULL){
  if(!(df$rank %in% c("unranked", "informal"))){
    if (rank_index(df$rank) >= rank_index(down_to)) {
      return(df)
    }
  }
  children <- request_metadata() |>
    filter("taxa" == df$taxon_concept_id) |>
    unnest() |>
    collect()
  if(nrow(children) < 1){
    return(df)
  }else{
    result <- children |> 
      mutate(name = str_to_title(children$name),
             taxon_concept_id = children$guid,
             parent_taxon_concept_id = children$parentGuid) |>
      select("name", "rank", "taxon_concept_id", "parent_taxon_concept_id")
    if(!is.null(constrain_ids)){
      result <- result |>
        constrain_id(constrain_to = constrain_ids) 
    }
    if(nrow(result) < 1){
      return(df)
    }else{
      result_list <- lapply(
        split(result, seq_len(nrow(result))), 
        function(a){drill_down_taxonomy(a, 
                                        down_to, 
                                        constrain_ids = constrain_ids)})
      return( 
        append(list(df), list(result_list)))
    }
  }
}

#' Internal function to check `identify` term is specified correctly
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_identify <- function(.query, error_call = caller_env()){
  if(is.null(.query$identify)){
    bullets <- c(
      "Argument `identify` is missing, with no default.",
      i = "Did you forget to specify a taxon?")
    abort(bullets, call = error_call)
  }
  
  if(nrow(.query$identify) > 1){
    number_of_taxa <- nrow(.query$identify)
    bullets <- c(
      "Can't provide tree more than one taxon to start with.",
      i = "atlas_taxonomy` only accepts a single taxon at a time.",
      x = glue("`identify` has length of {number_of_taxa}.")
    )
    abort(bullets, call = error_call)
  }
}

#' Internal function to check `identify` term is specified correctly
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_down_to <- function(.query, error_call = caller_env()){
  if (is.null(.query$filter$value)) {
    bullets <- c(
      "Argument `rank` is missing, with no default.",
      i = "Use `show_all(ranks)` to display valid ranks",
      i = "Use `filter(rank == chosen_rank)` to specify a rank"
    )
    abort(bullets, call = error_call)
  }
  
  down_to <- tolower(.query$filter$value) 
  if(!any(show_all_ranks()$name == down_to)){
    bullets <- c(
      "Invalid taxonomic rank provided.",
      i = "The rank provided to `rank` must be a valid taxonomic rank.",
      x = glue("{down_to} is not a valid rank.")
    )
    abort(bullets, call = error_call)
  }  
}

#' Internal function to only return GUIDs that match particular criteria
#' @importFrom purrr list_transpose
#' @importFrom dplyr filter
#' @noRd
#' @keywords Internal
constrain_id <- function(df, constrain_to){
  check_list <- lapply(constrain_to, function(a){grepl(a, df$taxon_concept_id)})
  check_result <- lapply(list_transpose(check_list), any) |>
    unlist()
  df |> filter(check_result)
}

# Return the index of a taxonomic rank- 
# lower index corresponds to higher up the tree
rank_index <- function(x) {
  all_ranks <- show_all_ranks()
  lapply(x, function(a){
    if (a %in% all_ranks$name) {
      return(all_ranks$id[all_ranks$name == a])  
    }else{
      NA
    }    
  }) |>
    unlist()
}
