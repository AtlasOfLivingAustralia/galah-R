#' Internal function to `collect` for type `data/distributions`
#' @importFrom sf st_as_sf
#' @importFrom sf st_as_sfc
#' @noRd
#' @keywords Internal
collect_distributions <- function(.query){
  result <- query_API(.query)
  # NOTE: below is consistent with `collect_distributions_metadata()` (+ geometry)
  result <- result |>
    bind_rows() |>
    select("gid", 
           "family", 
           "genus_name", 
           "scientific", 
           "common_nam",
           "lsid",
           "area_name",
           "area_km",
           "data_resource_uid",
           "geometry") |>
    rename(
      "id" = "gid", # this is chosen as ID because it is called by later APIs
      "genus" = "genus_name",
      "species" = "scientific",
      "taxon_concept_id" = "lsid",
      "label" = "area_name",
      "common_name" = "common_nam")  |>
    mutate("common_name" = trimws(.data$common_name))
  result$geometry <- st_as_sfc(result$geometry, crs=4326)
  return(st_as_sf(result))
}