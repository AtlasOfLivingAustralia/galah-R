#' Internal function to parse a `query_set` into a single `query`
#' @param x a `query_set`
#' @noRd
#' @keywords Internal
parse_query <- function(x){
  switch(x$type,
         "data/occurrences-count-groupby" = parse_occurrences_count(x),
         "data/occurrences-count" = parse_occurrences_count(x),
         "data/species-count" = parse_species_count(x),
         # "-unnest" functions require some checks
         "metadata/profiles-unnest" = parse_profile_values(x),  # check this
         # some "metadata/" functions require pagination under some circumstances
         "metadata/lists" = parse_lists(x), # always paginates
         x # remaining "metadata/" functions are passed as-is # fixme to make a new object type
  )
}