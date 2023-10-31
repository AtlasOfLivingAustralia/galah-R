#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_apis <- function(q_obj){
  result <- q_obj$data |>
    parse(text = _) |> 
    eval()
  attr(result, "call") <- "apis"
  attr(result, "region") <- pour("atlas", "region")
  result
}

#' Internal function to `collect()` assertions
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_assertions <- function(q_obj){
  if(!is.null(q_obj$data)){
    result <- q_obj$data |>
      parse(text = _) |> 
      eval()
    attr(result, "call") <- "assertions" # needed for `show_values()` to work
    attr(result, "region") <- pour("atlas", "region") # needed for caching to work
  }else{
    result <- lapply(query_API(q_obj), 
                     function(a){a[names(a) != "termsRequiredToTest"]}) |>
      bind_rows()
    names(result) <- rename_columns(names(result), type = "assertions")
    result <- result[wanted_columns("assertions")]
    result$type <- "assertions"
    attr(result, "call") <- "assertions" # needed for `show_values()` to work
    attr(result, "region") <- pour("atlas", "region") # needed for caching to work    
    check_internal_cache(assertions = result)
  }
  result
}

#' Internal function to `collect()` atlases
#' @noRd
#' @keywords Internal
collect_atlases <- function(q_obj){
  result <- q_obj$data |>
    parse(text = _) |> 
    eval()
  attr(result, "call") <- "atlases"
  attr(result, "region") <- pour("atlas", "region")
  result
}

#' Internal function to `collect()` collections
#' @importFrom dplyr bind_rows
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_collections <- function(q_obj){
  if(is_gbif()){
    result <- query_API(q_obj)
    if(any(names(result) == "results")){ # happens when `filter()` not specified
      # Note: This assumes only one API call; will need more potentially
      result <- pluck(result, "results")
    }
    result <- flat_lists_only(result) |>
      bind_rows()
  }else{
    result <- query_API(q_obj) |> 
      bind_rows() 
    result_reordered <- relocate(result, "uid") 
    result <- result_reordered |> rename("id" = "uid") 
  }
  attr(result, "call") <- "collections"
  attr(result, "region") <- pour("atlas", "region")
  result
}

#' Internal function to remove `list()` entries inside lists
#' This supports passing to `bind_rows()`, but loses data
#' @noRd
#' @keywords Internal
flat_lists_only <- function(x){
  lapply(x, 
         function(a){
           lapply(a, function(b){
             if(is.list(b)){
               NULL
             }else{
               b
             }
           })
         })
}

#' Internal function to `collect()` datasets
#' @importFrom dplyr bind_rows
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @noRd
#' @keywords Internal
collect_datasets <- function(q_obj){
  if(is_gbif()){
    result <- query_API(q_obj)
    if(any(names(result) == "results")){ # happens when `filter()` not specified
      # Note: This assumes only one API call; will need more potentially
      result <- pluck(result, "results")
    }
    result <- result |>
      flat_lists_only() |>
      bind_rows()
  }else{
    result <- query_API(q_obj)
    result <- result |> 
      bind_rows()
    result <- result |> 
      relocate("uid") |>
      rename("id" = "uid")
  }
  attr(result, "call") <- "datasets"
  attr(result, "region") <- pour("atlas", "region") 
  result 
}

#' Internal function to `collect()` fields
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_fields <- function(q_obj){
  if(is_gbif()){
    result <- q_obj$data |>
      parse(text = _) |> 
      eval()
    attr(result, "call") <- "fields"
    attr(result, "region") <- pour("atlas", "region")
    result
  }else{
    if(!is.null(q_obj$url)){ # i.e. there is no cached `tibble`
      result <- query_API(q_obj) |>
        bind_rows() 
      result <- result |>
        mutate(id = result$name) |>
        select(all_of(wanted_columns("fields"))) |>
        mutate(type = "fields") |>
        bind_rows(galah_internal_archived$media,
                  galah_internal_archived$other)
      attr(result, "call") <- "fields"
      attr(result, "region") <- pour("atlas", "region")
      check_internal_cache(fields = result)
      result
    }else{ # this should only happen when `data` slot is present in place of `url`
      check_internal_cache()[["fields"]]
    }
  }
}

#' Internal function to `collect()` licences
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_licences <- function(q_obj){
  result <- query_API(q_obj) 
  
  if (any(duplicated(names(result[[1]])))) { # remove duplicate columns (i.e. Spain atlas)
    result <- lapply(result, function(x) x[unique(names(x))])
  }
  
  result <- result |> 
    bind_rows() 
  result <- result |>
    select(all_of(c("id", "name", "acronym", "url"))) |> 
    arrange(result$id)
  attr(result, "call") <- "licences"
  attr(result, "region") <- pour("atlas", "region") 
  result
}

#' Internal function to `collect()` lists
#' @importFrom dplyr bind_rows
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_lists <- function(q_obj){
  if(inherits(q_obj$url, "data.frame")){
    result <- lapply(query_API(q_obj), 
                     function(a){a$lists}) |>
      bind_rows()    
  }else{
    result <- query_API(q_obj) |>
      pluck("lists") |>
      bind_rows()
  }
  attr(result, "call") <- "lists"
  attr(result, "region") <- pour("atlas", "region") 
  result
}

#' Internal function to `collect()` profiles
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_profiles <- function(q_obj){
  if(!is.null(q_obj$url)){
    result <- query_API(q_obj) |>
      bind_rows() 
    result <- result |>
      filter(!duplicated(result$id)) |>
      arrange("id") |>
      select(all_of(wanted_columns(type = "profile")))
    attr(result, "call") <- "profiles"
    attr(result, "region") <- pour("atlas", "region") 
    check_internal_cache(show_all_profiles = result)
    result
  }else{
    check_internal_cache()[["profiles"]]
  }
}

#' Internal function to `collect()` providers
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @noRd
#' @keywords Internal
collect_providers <- function(q_obj){
  if(is_gbif()){
    result <- query_API(q_obj)
    if(any(names(result) == "results")){ # happens when `filter()` not specified
      # Note: This assumes only one API call; will need more potentially
      result <- pluck(result, "results")
    }
    result <- result |>
      flat_lists_only() |>
      bind_rows()
  }else{
    result <- query_API(q_obj)
    result <- result |> 
      bind_rows()
    result <- result |> 
      relocate("uid") |> 
      rename("id" = "uid")
  }
  attr(result, "call") <- "providers"
  attr(result, "region") <- pour("atlas", "region")
  result
}


#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_ranks <- function(q_obj){
  result <- q_obj$data |>
    parse(text = _) |> 
    eval()
  attr(result, "call") <- "ranks"
  attr(result, "region") <- pour("atlas", "region")
  result
}

#' Internal function to `collect()` reasons
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_reasons <- function(q_obj){
  if(!is.null(q_obj$url)){
    result <- query_API(q_obj) |> 
      bind_rows() 
    result <- result |>
      filter(!result$deprecated) |>
      select(all_of(wanted_columns("reasons"))) |>
      arrange("id")
    attr(result, "call") <- "reasons"
    attr(result, "region") <- pour("atlas", "region") 
    check_internal_cache(reasons = result)
    result
  }else{
    check_internal_cache()[["reasons"]]
  }
}
