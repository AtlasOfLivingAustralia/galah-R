#' Biodiversity Data from the GBIF Node Network
#'
#' @description
#' The Global Biodiversity Information Facility (GBIF; <https://www.gbif.org>)
#' provides tools to enable users to find, access, combine and visualise 
#' biodiversity data. `galah` enables the R community to directly access data and 
#' resources hosted by GBIF and several of it's subsidiary organisations, known
#' as 'nodes'. The basic unit of observation stored by these infrastructures is 
#' an **occurrence** record, based on the Darwin Core' data standard 
#' (<https://dwc.tdwg.org>); however `galah` also enables users to locate and 
#' download taxonomic information, or associated media such images or sounds, 
#' all while restricting their queries to particular taxa or locations. Users 
#' can specify which columns are returned by a query, or restrict their results 
#' to observations that meet particular quality-control criteria. 
#'
#' For those outside Australia, 'galah' is the common name of
#' *Eolophus roseicapilla*, a widely-distributed Australian bird species.
#' @name galah
#' @docType package
#' @section Functions:
#' **Piping functions**
#'
#'   * [galah_call()] or \code{\link[=request_data]{request_()}} et al.  Start to build a data query
#'   * \code{\link[=collapse_galah]{collapse()}} Generate a query
#'   * \code{\link[=compute_galah]{compute()}} Compute a query
#'   * \code{\link[=collect_galah]{collect()}} Retrieve a database query
#'   
#' **Lazy data manipulation**
#' 
#'   * \code{\link[=identify.data_request]{identify()}} or [galah_identify()] Search for taxonomic identifiers
#'   * \code{\link[=filter.data_request]{filter()}} or [galah_filter()]Filter records
#'   * \code{\link[=select.data_request]{select()}} or [galah_select()]Fields to report information for
#'   * \code{\link[=group_by.data_request]{group_by()}} or [galah_group_by()] Fields to group counts by
#'   * \code{\link[=st_crop.data_request]{st_crop()}} or [galah_geolocate()] Specify a location
#'   * [apply_profile()] or [galah_apply_profile()] Restrict to data that pass predefined checks (ALA only)
#'   * \code{\link[=slice_head.data_request]{slice_head()}} Choose the first n rows of a download
#'   * \code{\link[=arrange.data_request]{arrange()}} Arrange rows of a query on the server side
#' 
#' **Download data**
#' 
#'   * [atlas_occurrences()] Download occurrence records
#'   * [atlas_counts()] or \code{\link[=count.data_request]{count()}} Count the number of records or species returned by a query
#'   * [atlas_species()] Download species lists
#'   * [atlas_taxonomy()] Return a section of the ALA taxonomic tree
#'   * [atlas_media()] View images and sounds available to download
#'   * [collect_media()] Download images and sounds
#'
#' **Look up information**
#'
#'   * [search_taxa()] Search for taxa using a text-search
#'   * [search_identifiers()] Search for taxa using taxonomic identifiers
#'   * [show_all()] & [search_all()] Data for generating filter queries
#'   * [show_values()] & [search_values()] Show or search for values _within_ 
#'   `fields`, `profiles`, `lists`, `collections`, `datasets` or `providers`
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
#' terminology. Each occurrence record contains taxonomic
#' information, and usually some information about the observation itself, such
#' as its location. In addition to this record-specific information, the living 
#' atlases append contextual information to each record, particularly data from 
#' spatial **layers** reflecting climate gradients or political boundaries. They
#' also run a number of quality checks against each record, resulting in
#' **assertions** attached to the record. Each piece of information
#' associated with a given occurrence record is stored in a **field**,
#' which corresponds to a **column** when imported to an
#' `R data.frame`. See `show_all(fields)` to view valid fields,
#' layers and assertions, or conduct a search using `search_all(fields)`.
#'
#' Data fields are important because they provide a means to **filter**
#' occurrence records;  i.e. to return only the information that you need, and
#' no more. Consequently, much of the architecture of `galah` has been
#' designed to make filtering as simple as possible. The easiest way to do this 
#' is to start a pipe with `galah_call()` and follow it with the relevant 
#' `dplyr` function; starting with `filter()`, but also including `select()`,
#' `group_by()` or others. Functions without a relevant `dplyr` synonym include
#' [galah_identify()]/`identify()` for choosing a taxon, or [galah_geolocate()]/
#' `st_crop()` for choosing a specific location. By combining different filters, 
#' it is possible to build complex queries to return only the most valuable 
#' information for a given problem.
#'
#' A notable extension of the filtering approach is to remove records with low
#' 'quality'. All living atlases perform quality control checks on all records 
#' that they store. These checks are used to generate new fields, that can then 
#' be used to filter out records that are unsuitable for particular applications. 
#' However, there are many possible data quality checks, and it is not always 
#' clear which are most appropriate in a given instance. Therefore, `galah` 
#' supports data quality **profiles**, which can be passed to 
#' [galah_apply_profile()] to quickly remove undesirable records. A full list of 
#' data quality profiles is returned by `show_all(profiles)`. Note this service 
#' is currently only available for the Australian atlas (ALA).
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom lifecycle badge
NULL
