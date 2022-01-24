#' Download biodiversity data from the Atlas of Living Australia
#'
#' @description
#' `galah` is an R interface to the Atlas of
#' Living Australia (ALA; <https://www.ala.org.au/>), 
#' a biodiversity data repository focussed
#' primarily on observations of individual life forms. 
#' It also supports access to some other 'living atlases' that use the same 
#' computational infrastructure. The
#' basic unit of data at ALA is an **occurrence** record, based on the
#' 'Darwin Core' data standard (<https://dwc.tdwg.org>). `galah` enables users to
#' locate and download species observations, taxonomic information, or
#' associated media such images or sounds, and to restrict their queries to
#' particular taxa or locations. Users can specify which columns are returned
#' by a query, or restrict their results to observations that meet particular
#' quality-control criteria. 
#'
#' @name galah
#' @docType package
#' @references For more information on the ALA API, visit <https://api.ala.org.au/>.
#' If you have any questions, comments or suggestions, please email
#' [support@ala.org.au](mailto:support@ala.org.au).
#'
#' @section Functions:
#' **Start a data query**
#'
#'   * [galah_call()] Start to build a data query
#' 
#' **Narrow your results**
#' 
#'   * [galah_identify()] Search for taxonomic identifiers
#'   * [galah_filter()] Filter records
#'   * [galah_select()] Fields to report information for
#'   * [galah_group_by()] Fields to group counts by
#'   * [galah_geolocate()] Specify a location
#'   * [galah_down_to()] Specify a taxonomic rank
#' 
#' **Download data**
#' 
#'   * [atlas_occurrences()] Download occurrence records
#'   * [atlas_counts()] Count the number of records or species returned by a query
#'   * [atlas_species()] Download species lists
#'   * [atlas_taxonomy()] Return a section of the ALA taxonomic tree
#'   * [atlas_media()] Download images and sounds
#'   
#' **Look up information**
#'
#'   * [search_taxa()] Search for taxa using a text-search
#'   * [search_identifiers()] Search for taxa using taxonomic identifiers
#'   * [search_fields()] Search for specific valid fields to filter queries
#'   * [search_field_values()] Search for possible values to filter by for a given field
#'   * [search_profile_attributes()] List the quality filters of a data quality profile
#'   * [show_all_fields()] List valid fields to filter or categorise queries
#'   * [show_all_profiles()] List data quality profiles
#'   * [show_all_reasons()] List valid download reasons
#'   * [show_all_atlases()] List supported international atlases
#'   * [show_all_ranks()] List available taxonomic ranks
#' 
#' **Manage cache**
#' 
#'   * [show_all_cached_files()] List previously cached files and their metadata
#'   * [clear_cached_files()] Clear previously cached files and their metadata
#' 
#' **Configure session**
#' 
#'   * [galah_config()] Package configuration options
#' 
#' **Cite**
#' 
#'   * [atlas_citation()] Citation for a dataset
#'
#' @section Terminology:
#'
#' To get the most value from `galah`, it is helpful to understand some
#' terminology used by the ALA. Each occurrence record contains taxonomic
#' information, and usually some information about the observation itself, such
#' as its location. In addition to this record-specific information, ALA
#' appends contextual information to each record, particularly data from spatial
#' **layers** reflecting climate gradients or political boundaries. ALA
#' also runs a number of quality checks against each record, resulting in
#' **assertions** attached to the record. Each piece of information
#' associated with a given occurrence record is stored in a **field**,
#' which corresponds to a **column** when imported to an
#' `R data.frame`. See [show_all_fields()] to view valid fields,
#' layers and assertions, or conduct a search using [search_fields()].
#'
#' Data fields are important because they provide a means to **filter**
#' occurrence records;  i.e. to return only the information that you need, and
#' no more. Consequently, much of the architecture of `galah` has been
#' designed to make filtering as simple as possible. 
#' Functions with the `galah_` prefix offer ways to shape your query 
#' call. Each `galah_` function allows the user to filter in a different way. 
#' Again, the function suffix reveals what each one does. `galah_filter`, 
#' `galah_select` and `galah_group_by` intentionally match `dplyr`'s `select()`, 
#' `filter()` and `group_by()` functions, both in their name and how they they are
#' used. For example, you can use [galah_select()] to choose what information
#' is returned as columns. Alternatively, you can use [galah_filter()] to filter
#' the rows. You can also choose specific taxa with [galah_identify()] or choose 
#' a specific location using [galah_geolocate()]. 
#' By combining different filter functions, it is possible to build complex 
#' queries to return only the most valuable information for a given problem.
#'
#' A notable extension of the filtering approach is to remove records with low
#' 'quality'. ALA performs quality control checks on all records that it stores.
#' These checks are used to generate new fields, that can then be used to filter
#' out records that are unsuitable for particular applications. However, there
#' are many possible data quality checks, and it is not always clear which are
#' most appropriate in a given instance. Therefore, `galah` supports ALA
#' data quality **profiles**, which can be passed to
#' [galah_filter()] to quickly remove undesirable records. A full
#' list of data quality profiles is returned by [show_all_profiles()].
#'
#' For those outside Australia, 'galah' is the common name of
#' *Eolophus roseicapilla*, a widely-distributed
#' Australian bird species.
#'
#' @section Package design:
#'
#' In most cases, users will be primarily interested in using `galah` to
#' return data from one of the living atlases. These functions are named with 
#' the prefix `atlas_`, followed by a suffix describing the information that 
#' they provide. For example, users that wish to download occurrence data can 
#' use the function [atlas_occurrences()]. Alternatively, users that wish to 
#' download data on each species (rather than on each occurrence record) can use
#' [atlas_species()] or download media content (largely images) 
#' using [atlas_media()]. Users can also assess how many records
#' meet their particular criteria using [atlas_counts()] and return a taxonomic 
#' tree for a specific clade from one level down to another level (e.g., from 
#' family to genus). All functions return a `data.frame` as their standard 
#' format, except [atlas_taxonomy()] which returns a `data.tree`.
#'
#' Functions in `galah` are designed according to a nested architecture. 
#' Users that require data should begin by locating the relevant `atlas_` 
#' function; the arguments within that function then call correspondingly-named 
#' `galah_` functions; specific values that can be interpreted by those `galah_`
#' functions are given by functions with the prefix `search_` or
#' `show_all_`; desired taxa can be also be identified using [search_taxa()] and 
#' passed within [galah_identify()] to the `taxa` argument of `atlas_` functions.
#'

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import assertthat sf httr
#' @importFrom crul HttpClient Paginator Async url_build url_parse
#' @importFrom data.table rbindlist
#' @importFrom data.tree Do Set Prune Aggregate FromListExplicit ToDataFrameTypeCol ToDataFrameTree as.Node
#' @importFrom digest digest
#' @importFrom jsonlite fromJSON
#' @importFrom stringr regex str_c str_detect str_extract str_locate
#' str_match str_match_all str_to_title
#' @importFrom stringr str_replace str_replace_all str_split str_trim str_match
#' @importFrom utils data packageVersion read.csv write.csv read.table str
#' unzip URLencode download.file setTxtProgressBar txtProgressBar tail
#' @importFrom wellknown lint
#' @importFrom lifecycle deprecated
#' @importFrom rlang abort warn inform caller_env
#' @importFrom glue glue glue_collapse
#' @importFrom tibble as_tibble tibble is_tibble
## usethis namespace: end
NULL
