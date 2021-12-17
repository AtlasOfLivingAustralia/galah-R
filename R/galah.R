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
#' **`Data`**
#' 
#'   * [atlas_counts()] Count the number of records or species returned by a query
#'   * [atlas_taxonomy()] Return a section of the ALA taxonomic tree
#'   * [atlas_species()] Download species lists
#'   * [atlas_occurrences()] Download occurrence records
#'   * [atlas_media()] Download images and sounds
#'   * [atlas_citation()] Citation for a dataset
#' 
#' **`Filter`**
#' 
#'   * [galah_filter()] Filter records
#'   * [galah_select()] Fields to report information for
#'   * [galah_group_by()] Fields to group counts by
#'   * [galah_geolocate()] Specify a location
#' 
#' **`Lookup`**
#' 
#'   * [search_taxa()] Search for taxonomic identifiers
#'   * [search_fields()] Free-text search for layers and fields
#'   * [show_all_profiles()] List data quality profiles
#'   * [show_all_ranks()] List available taxonomic ranks
#'   * [show_all_reasons()] List valid download reasons
#'   * [show_all_atlases()] List supported international atlases
#'   * [show_all_fields()] List fields to filter or categorise queries
#'   * [find_field_values()] List possible values for a given field
#'   * [find_profile_attributes()] List filters included in a data quality profile
#' 
#' **`Cache management`**
#' 
#'   * [find_cached_files()] List previously cached files and their metadata
#'   * [clear_cached_files()] Clear previously cached files and their metadata
#' 
#' **`Help`**
#' 
#'   * [galah_config()] Package configuration options
#' 
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
#' `R data.frame`. See [search_fields()] to view valid fields,
#' layers and assertions.
#'
#' Data fields are important because they provide a means to **filter**
#' occurrence records;  i.e. to return only the information that you need, and
#' no more. Consequently, much of the architecture of `galah` has been
#' designed to make filtering as simple as possible, by using functions with the
#' `galah_` prefix. Each `galah_` function allows the user to filter
#' in a different way, and again the function suffix contains this information.
#' For example, you can choose which taxonomic groups are included using
#' [search_taxa()], or a specific location using
#' [galah_geolocate()]. By combining different filter functions, it
#' is possible to build complex queries to return only the most valuable
#' information for a given problem.
#'
#' A notable extention of the filtering approach is to remove records with low
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
#' the prefix `atlas_`,
#' followed by a suffix describing the information that they provide. For
#' example, we anticipate that users will wish to download occurrence data,
#' which can be achieved using the function [atlas_occurrences()].
#' However, it is also possible to download data on species via
#' [atlas_species()], or media content (largely images) via
#' [atlas_media()]. Alternatively, users can assess how many records
#' meet their particular criteria using [atlas_counts()]. All 
#' functions return a `data.frame` as their standard format, except 
#' [atlas_taxonomy()] which returns a `data.tree`.
#'
#' Functions in `galah` are designed
#' according to a nested architecture. Users that require data should begin by
#' locating the relevant `atlas_` function; the arguments within that
#' function then call correspondingly-named `galah_` functions; and
#' finally the specific values that can be interpreted by those `galah_`
#' functions are given by functions with the prefix `search_` or
#' `find_`. So, to limit occurrence downloads to a specific taxonomic
#' group, for example, you pass the result of [search_taxa()] to the
#' `taxa` argument of [atlas_occurrences()].
#'

NULL
