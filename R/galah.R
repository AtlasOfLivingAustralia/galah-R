#' \pkg{galah}
#'
#' \code{galah} is an R interface to biodiversity data hosted by the
#' Atlas of Living Australia (\url{https://www.ala.org.au/}). It enables
#' users to locate and download species observations, taxonomic information,
#' or associated media such images or sounds, and to restrict their queries
#' to particular taxa or locations. Users can specify precisely what
#' columns are returned by each query, or restrict their results to observations
#' that meet particular quality-control criteria. All functions return a
#' \code{data.frame} as their standard format.
#'
#'
#' If you have any questions, comments or suggestions, please email \href{mailto:support@ala.org.au}{support@ala.org.au}.
#'
#' @name galah
#' @docType package
#' @references For more information on the ALA API, visit \url{https://api.ala.org.au/}
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
#' \strong{Data}
#' \itemize{
#'   \item\code{\link{ala_occurrences}} Download occurrence records
#'   \item\code{\link{ala_species}} Download occurrence records
#'   \item\code{\link{ala_counts}} Summary statistics for occurrence records
#'   \item\code{\link{ala_media}} Download images and sounds
#' }
#' \strong{Filter}
#' \itemize{
#'   \item\code{\link{select_taxa}} Taxon information
#'   \item\code{\link{select_filters}} Filter records
#'   \item\code{\link{select_locations}} Specify location
#'   \item\code{\link{select_columns}} Columns to return in occurrence download
#' }
#' \strong{Lookup}
#' \itemize{
#'   \item\code{\link{find_profiles}} List data quality profiles
#'   \item\code{\link{find_profile_attributes}} List filters included in a data quality profile
#'   \item\code{\link{find_fields}} List occurrence record fields
#'   \item\code{\link{find_field_values}} List possible values for a given field
#'   \item\code{\link{find_layers}} List spatial layers
#'   \item\code{\link{find_reasons}} List valid download reasons
#' }
#' \strong{Help}
#' \itemize{
#'   \item\code{\link{ala_config}} Package configuration options
#'   \item\code{\link{ala_citation}} Citation for a dataset
#' }

NULL
