#' \pkg{galah}
#'
#' This project enables the R community to access data and tools hosted by the
#' Atlas of Living Australia. The goal of the project is
#' to enable basic species and related information to be queried and output in
#' standard formats for R. galah is based around the extensive web services
#' provided by the Atlas; see the API link below.
#'
#' If you have any questions or suggestions, please email support@ala.org.au.
#'
#' @name galah
#' @docType package
#' @references \url{https://api.ala.org.au/}
#' @import assertthat sp sf httr
#' @importFrom crul HttpClient Paginator
#' @importFrom digest digest
#' @importFrom jsonlite fromJSON
#' @importFrom stringr regex str_c str_detect str_extract str_locate
#' str_match str_match_all
#' @importFrom stringr str_replace str_replace_all str_split str_trim str_match
#' @importFrom utils data packageVersion read.csv write.csv read.table str
#' unzip URLencode download.file setTxtProgressBar txtProgressBar tail
#' @importFrom wellknown lint
#' 
#' @section Functions:
#' \strong{Data functions}
#' \itemize{
#'   \item\code{\link{ala_taxa}} Taxon information
#'   \item\code{\link{ala_occurrences}} Download occurrence records
#'   \item\code{\link{ala_counts}} Summary statistics for occurrence records
#'   \item\code{\link{ala_media}} Download images and sounds
#' }
#' \strong{Helper functions}
#' \itemize{
#'   \item\code{\link{ala_config}} Package configuration options
#'   \item\code{\link{ala_citation}} Citation for a dataset
#' }
#' \strong{Select_ functions}
#' \itemize{
#'   \item\code{\link{select_filters}} Filter records
#'   \item\code{\link{select_locations}} Specify location
#'   \item\code{\link{select_columns}} Columns to return in occurrence download
#' }
#' \strong{Find_ functions}
#' \itemize{
#'   \item\code{\link{find_profiles}} List data quality profiles
#'   \item\code{\link{find_profile_attributes}} List filters included in a data quality profile
#'   \item\code{\link{find_fields}} List occurrence record fields
#'   \item\code{\link{find_field_values}} List possible values for a given field
#'   \item\code{\link{find_layers}} List spatial layers
#'   \item\code{\link{find_reasons}} List valid download reasons
#' }
NULL
