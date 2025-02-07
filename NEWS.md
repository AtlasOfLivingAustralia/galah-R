# galah 2.1.1

### Minor improvements
* New vignette to demonstrate methods that support reproducibility
* New function `read_zip()` to reimport downloaded files
* Support `group_by()` in occurrence queries to allow facet downloads by any variable (#195, #258)
* Improvements to `atlas_citation()` for improved clarity

### Bug fixes
* Improved documentation to use `galah_filter()` to specify a `taxon_concept_id` rather than `galah_identify()` (#245)
* Adding a `field` without data breaks occurrence downloads (#248)
* Queries that filter using both `!` and `%in%` parse correctly (#251)
* `show_all(lists)` no longer truncates results to first 500 rows (#252)
* `atlas_counts()` no longer errors when `group_by()` is set but record count = 0 (#254)
* Empty tibbles returned by `atlas_species()` no longer return different column names to queries that return a result (#255)

# galah 2.1.0

### Image downloads
galah now supports media downloads for all atlases. The only exceptions are GBIF and France, for whom these APIs are not supported (yet)

### Minor improvements
* Reorganise help files for improved clarity, largely following `dplyr` syntax
* Support data profiles for Sweden and Spain
* Species downloads (via `atlas_species()`) now work for Sweden, France, and Spain (#234)
* `select()` now works for species downloads (i.e. via `atlas_species()`; #185, #227)
* Temporarily remove Estonian atlas (https://elurikkus.ee) during system upgrades

### Bug fixes
* Fix bugs in `filter`, `group_by` etc. not recognising fields (#237)
* Swap to new APIs for Australia (#163) and Austria (#231, #242)
* Re-add taxonomic help under `?taxonomic_searches` (#241)

# galah 2.0.2

### Minor improvements
* Experimental `galah_geolocate(type = "radius")` added. Supports filtering by point location and radius (in km) (#216)
* Support `galah_geolocate()` and associated sub-functions for GBIF queries
* `galah_filter()` no longer fails when assertions are specified in `galah_filter()` (#199)
* Improved behaviour and robustness of `atlas_species()`, particularly for other atlases (#234)
* Improved behavior of `select()`, including supporting `atlas_species()` and adding new `group = "taxonomy"` option (#218)
* Updated namematching services for SBDI (Sweden) (#210)
* Add onLoad message so user is clear which organisation is being queried

### Bug fixes
* `collect_media()` no longer fails when a thumbnail is missing (#215)
* `galah_filter()` parses apostrophes correctly in value names (#214)
* `group_by() |> atlas_counts()` no longer truncates rows at 30 (#223, #198) 
* Fix bug where `search_values()` did not return matched values
* `show_values()` & `atlas_counts()` return correctly formatted values (#233)
* `atlas_occurrences()` no longer overwrites returned field names with user-supplied ones
* `galah_apply_profile()` now works as expected
* List items are no longer truncated when using `show_values()` (#235)


# galah 2.0.1

### Minor improvements
`collapse()` now returns a `query` object, rather than a `query_set`, 
and gains a `.expand` argument to optionally append a `query_set` for debugging
purposes (#217).

### Bug fixes
* Avoid triggering multiple errors when galah_identify() returns no matched taxa (#219)
* Improve clarity of quick start guide vignette (#221)
* show_values() no longer truncates rows to 30 (#222)
* Column ID name returned by search_all(lists) now matches the correct field ID for galah_filter() (#225)

# galah 2.0.0

### Object-oriented programming
* galah 2.0.0 is now built around object-oriented programming principles. This architectural change makes query building in galah more modular and transparent. As a result, galah 2.0.0 allows for easier debugging and gives users options for more advanced query building (for more information, see "Object-oriented programming" vignette on [galah website](https://galah.ala.org.au/R/)) (#183).

### `collapse()`, `compute()`, `collect()`
* New underlying architecture behind every function that pings an API in galah separates query building into 3 stages: Convert an object to a `query_set` that lists all APIs that will be pinged (`collapse()`), send the queries to required APIs (`compute()`), and return data as a `tibble` (`collect()`) (#183).
* New architecture solves timing-out issue when downloading large numbers of records (#180, #192)
* `galah_filter()`, `galah_select()` and related functions now evaluated lazily; no API calls are made until `compute()` is called, meaning that earlier programming stages are faster and easier to debug.

### Major improvements to `galah_filter()`
* `galah_filter()` has been upgraded to use a hierarchical parsing architecture suggested by [Advanced R](https://adv-r.hadley.nz/expressions.html). As a result, `galah_filter()` is faster and evaluates expressions more consistently (#196, #169)
* `galah_filter()` now supports `is.na`, `!`, `c()` & `%in%` (#196)

### Minor improvements
* The [potions package](https://potions.ala.org.au/) underlies `galah_config()` for better options management (#193)
* Addition of `slice_head()` and `desc()` as masked functions to use in galah `atlas_counts()` query.
* New vignettes added for advanced taxonomic, spatial and temporal filtering (#42)

### Bug fixes
* Fixed parsing of `|` in `galah_filter()` (#169)
* `show_values()` errors nicely when API is down (#184)
* Sporadic `atlas$region` error when loading galah fixed with potions package implementation (#178)
* DOI is no longer missing as an attribute when `atlas_occurrences(mint_doi = TRUE)` (#182)
* Fixed bug where the order of fields in `group_by()` sometimes caused an error (#201)
* Fixed parsing of ampersands (`&`) in query results (#203)
* galah builds correct `data_request` object when wrapped by a function (#207)

# galah 1.5.4

Patch release to fix minor issues on some `devel` systems on CRAN.

# galah 1.5.3

Minor release to address CRAN issues. Last release before 2.0.0.

# galah 1.5.2

Minor release to resolve issues on CRAN, and a few recent bugs.

### Bug fixes
- Prevent error when providing a `tibble` as input to `search_taxa()` (e.g., to resolve homonyms, #168)
- Better error message when email address is required, but not given (#179)
- Add an informative message when users call `galah_select()` while atlas = GBIF (which is not supported; #181)
- Ensure DOIs are added to downloads when requested (#182)
- Improve tests to avoid flagging issues on CRAN when one or more atlases are down (#184)
- Resolve problem where some queries were replaced by `...` in `galah_filter()` (#186)


# galah 1.5.1

### Mask function names from other packages
An experimental feature of version 1.5.1 is the ability to call functions from other packages (#161), as synonyms for `galah_` functions. These are:

* `identify()` (`{graphics}`) as a synonym for `galah_identify()`
* `select()` (`{dplyr}`) as a synonym for `galah_select()`
* `group_by()` (`{dplyr}`) as a synonym for `galah_group_by()`
* `slice_head()` (`{dplyr}`) as a synonym for the `limit` argument in `atlas_counts()`
* `st_crop()` (`{sf}`) as a synonym for `galah_polygon()`
* `count()` (`{dplyr}`) as a synonym for `atlas_counts()`

These are implemented as S3 methods for objects of class `data_request`, which are created by `galah_call()`. Hence new function names only work when piped after `galah_call()`.

### Experimental support for GBIF queries
The Global Biodiversity Information Facility (GBIF) is the umbrella organisation to which all other atlases supply data. Hence it is logical to be able to query GBIF and it's "nodes" (i.e. the living atlases) via a common API. Supported functions are:

* `search_taxa` and `galah_identify` for name matching
* `show_all(fields)` and `show_all(assertions)`
* `show_all()` calls that give 'collections' information are limited to 20 records by default, as GBIF datasets are often huge. `search_all()` is generally more reliable
* `show_values()` for any GBIF field
* `galah_filter` and `galah_group_by` (and therefore `filter` and `group_by()`, see above), but NOT `galah_select`.
* `atlas_counts()` (and therefore `count()`, see above)
* `atlas_occurrences()` & `atlas_species()`; both are implemented via the 'downloads' system, meaning that queries can be larger, but may be slow

The current implementation is experimental and back-end changes are expected in future. Users who require a more stable implementation should use the {rgbif} package.

### Minor improvements
* `galah_config()` gains a `print` function, and now uses fuzzy matching for the `atlas` field to match to region, organisation or acronym (as defined by `show_all(atlases)`). An example use case is to match to organisations via acronyms, e.g. `galah_config(atlas = "ALA")`.
* Improved support for data from Spain via [gbif.es](https://gbif.es) (name-matching, lists, spatial)
* Swapped provider for data from France; formerly [gbif.fr](http://www.gbif.fr), now [OpenObs](https://openobs.mnhn.fr), as per advice from maintainers
* Reading data from disk now uses `readr::read_csv` in place of `utils::read.csv` for improved speed
* `show_all` (and associated sub-functions) gain a `limit` argument, set to NULL (i.e. no limit) by default
* `galah` no longer imports `{data.table}`, since the only function previously used from that package (`rbindlist`) is duplicated by `dplyr::bind_rows`
* Help files are now built without markdown for improved speed (mainly while building)

## Bug fixes:
* New function `url_paginate()` to handle cases where pagination is needed, but total data length is unknown (e.g. `show_all_lists()`, #170).
* `galah_select(group = "assertions")` is always enacted properly by `atlas_occurrences`, and won't lead to overly long urls (#137). When called without any other field names, `recordID` is added to avoid triggering the 'default' set of columns.
* `atlas_species` works again after some minor changes to the API; but requires a registered email to function


# galah 1.5.0

### Expanded support for querying other International Living Atlases

* Support for complex queries to 10 Living Atlases, including France, Guatemala and Sweden. Complex queries can be constructed using `galah_call()`, filtered with `galah_` functions, and downloaded with `atlas_` functions. Previously, this functionality was only possible with queries to the ALA (#126)

### `collect_media()`

* `atlas_media()` has been improved to use 2 simplified functions to show & download media (#145, #151):
  1) **Show available media**: `atlas_media()` returns a `tibble` of available media files 
  2) **Download media**: `collect_media()` downloads the list of media from `atlas_media()` to a local machine 
* Download image thumbnails by specifying `type = "thumbnails"` in `collect_media()` (#140)

### Updates to `galah_geolocate()`

* `galah_geolocate()` now supports filtering queries using polygons and bounding boxes. Overall improvements and bug fixes to `galah_geolocate()` through new internal functions `galah_polygon()` and `galah_bbox()` (#125)

### `show_all()`, `search_all()` & `show_values()`, `search_values()`

* Experimental functions `show_all()` and `search_all()` are flexible look-up functions that can search for all information in {galah}, rather than by separate `search_`/`show_all_` functions (e.g. `search_fields()`, `search_atlases()`, `show_all_fields()`, `show_all_reasons()`, etc) (#127, #132)
* Added look-up support for collections, data providers, data resources, licenses, APIs and species lists (e.g. list of endangered species) (#126, #127, #132, #151)
* Checking for valid values _within_ fields, profiles and species lists are improved with functions `show_values()` & `search_values()` (#131)

### Minor improvements

* Apply data quality profiles in a pipe with the `galah_apply_profile()` function (#130)
* Improved internal consistency of `galah_` functions (#133)
* `galah_geolocate()` no longer depends on archived {wellknown} package (#141)
* Added support for queries to exclude/include missing values (e.g. `galah_filter(species != "")` or `galah_filter(species == "")` (#143)
* Re-download a previously-minted DOI with `collect_doi()` (#140)
* More checks to ensure galah fails gracefully when an API fails (#157)

### Bug fixes

* `galah_select()` no longer adds "basic" group of columns automatically (#128)
* `galah_config()` doesn't display incorrect `preserve = TRUE` message (#136)
* Fixes error when selecting assertion columns with `galah_select()` (#137)
* `atlas_counts()` and `atlas_occurrences()` no longer return different record numbers when a field is empty (#138)
* `atlas_media()` results no longer differ to results returned by `galah_filter()` & `atlas_counts()` (#151)


# galah 1.4.0

### Revamped syntax

* `ala_` functions are renamed to use the prefix `atlas_`. This change reflects their functionality with international atlases (i.e., `atlas_occurrences`, `atlas_counts`, `atlas_species`, `atlas_media`, `atlas_taxonomy`, `atlas_citation`) (#103)
* `select_taxa` is replaced by 3 functions: `galah_identify`, `search_taxa` and `search_identifiers`. `galah_identify` is used when building data queries, whereas `search_taxa` and `search_identifiers` are now exclusively used to search for taxonomic information. Syntax changes are intended to reflect their usage and expected output (#112, #122)
* `select_` functions are renamed to use the prefix `galah_`. Specifically, `galah_filter`, `galah_select` and `galah_geolocate` replace `select_filters`, `select_columns` and `select_locations`. These syntax changes reflect a move towards consistency with `dplyr` naming and functionality (#101, #108)
* `find_` functions that provide a listing of all possible values renamed to `show_all_` (i.e., `show_all_profiles`, `show_all_ranks`, `show_all_atlases`, `show_all_cached_files`, `show_all_fields`, `show_all_reasons`). `find_` functions that require and input and return specific results renamed to `search_` (i.e., `search_field_values`, `search_profile_attributes`) (#112, #113) 

### `galah_group_by`

* Group fields using `galah_group_by()`, which groups and summarises record counts based on categorical field values, similar to `dplyr::group_by()` (#90, #95)

### `galah_down_to`

* Select which taxonomic level a taxonomic tree will go down to with `galah_down_to()` + `atlas_taxonomy()`, which uses tidy evaluation like other `galah_` functions (#101, #120)

### Pipe queries using `galah_call`

* Build data queries using piping syntax (i.e., `|>`, `%>%`) by first using `galah_call()`, narrowing queries with `galah_` functions and finishing queries with an `atlas_` function (#60, #120). 
* S3 methods are now implemented to functions to allow for piping (#40)

### Minor improvements

* Improved error messages using {glue} and {rlang} (#117)
* Revamped syntax functions return output as tibbles (#110, #118)
* Pass vectors to `galah_filter` (#91, #92)
* Cache valid fields for faster field look up (#73, #116)
* New vignettes for updated syntax (#104, #105), plus improvements to previous vignettes.
* Updated R Markdown-style documentation and added warnings for deprecated functions (#113, #121)

### Bug fixes

* galah no longer returns error when ALA system is down and/or API fails (#102, #119)
* `search_taxa` returns correct IDs for search terms with parentheses (#96)
* `search_taxa` returns best-fit taxonomic result when ranks are specified in `data.frame` or `tibble` (#115)


# galah 1.3.1

### `search_taxonomy()` renamed to `ala_taxonomy()`

* bug fix: `ala_taxonomy` no longer fails for nodes ranked as `informal` or `unranked` (#86)
* this function now returns a tree built using the `data.tree` package
* change in function name required for greater consistency with other data-providing functions in `galah`

### Vignettes

* vignettes are now pre-compiled to avoid failing on CRAN (#85)
* expanded vignette on navigating taxonomic information (#42)

# galah 1.3.0

### `galah_config()`
* `ala_config()` has been renamed to `galah_config()` to improve internal 
consistency (#68)

### `search_taxonomy()`
* `search_taxonomy()` provides a means to search for taxonomic names and check 
the results are 'correct' before proceeding to download data via 
`ala_occurrences()`, `ala_species()` or `ala_counts()` (e.g., not ambiguous or 
homonymous) (#64 #75)
* `search_taxonomy()` returns information of author and authority of taxonomic 
names (#79)
* `search_taxonomy()` consistently orders column names, including in correct 
taxonomic order by rank (#81)

### Caching helper functions
* `find_cached_files()` lists all user cached files and stored metadata (#57)
* `clear_cached_files()` removes previously cached files and stored metadata 
(#71)
* `ala_counts()`, `ala_occurrences()`, `ala_media()` and `ala_species()` now 
have `refresh_cache` argument to remove previously cached files and replace with 
the current query (#71)

### Minor improvements
* Cache files are saved in RDS format, making query attributes easier to find, 
including data DOI, search url (#55, #32, #28)
* `ala_media()` caches media metadata if `galah_config(caching = TRUE)`
* `search_fields()` allows the user to pass a `qid` as an argument (#59)
* Users can now optionally skip filter and count validation checks to spatial 
and biocache web services by setting `galah_config(run_checks = FALSE)`. This 
helps users avoid slowing down data request download speeds when many requests 
are made in quick succession via `galah_filter()` or `ala_occurrences()` (#61,
#80)
* `ala_counts()`, `select_columns()` and `search_fields()` now use `match.arg` 
to approximate strings through fuzzy matching (#66)
* Better handling of cache errors and improved error messages (#70)

### Bug fixes
* `select_columns(group = 'assertions')` now sends `qa = includeall` to ALA web 
service API to return all assertion columns (#48)
* `ala_occurrences()` returns data DOI when `ala_occurrences(mint_doi = TRUE)` 
and re-downloads data when called multiple times (#56)
* `ala_occurrences()` no longer converts field names with all-CAPS to camelCase 
(#62)


# galah 1.2.0

### Living Atlases
* `ala_config()` allows users to specify an international Atlas to download data
from (#21)

### Minor improvements
* `ala_media()` includes the file path to the downloaded media in the
returned metadata (#22)
* Data returned from `ala_occurrences()` contains the `search_url` used to
download records; this takes the user to the website search page (#32)
* `ala_species()` provides a more helpful error if no species are found (#39)
* Data quality filters are created using the specific web service argument,
rather than constructing filters from the attributes (#37)
* `select_taxa()` has an optional `all_ranks` argument to return intermediate
rank information (#35)

### Bug fixes
* R > 4.0.0 is now required (#43, #45)
* `select_taxa()` behaves as expected when character strings of 32 or 36
characters are provided (#23)
* Caching functionality for `ala_occurrences()` uses the `columns` as expected
(#30)
* `galah_filter()` negates assertion filters when required, fixing the issue
of assertion values being ignored (#27)
* `select_taxa()` no longer throws an error when queries of more than one term
have a differing number of columns in the return value (#41)
* `ala_counts()` returns data.frame with consistent column classes when 
a `group_by` parameter is called multiple times and `ala_config(caching = TRUE)` 
(#47)
* `ala_` functions fail gracefully if a non-id character string is passed (#49)


# galah 1.1.0

### Downloading media
* `ala_media()` now takes the same `select_` arguments as other `ala_`
functions (#18)
* Filtering by media metadata e.g. licence type is possible (#19)
* `search_fields` now has `media` as a `type` argument option
* Performance improvement in download times (#13)
* Progress bar displayed for downloads when `verbose == TRUE` (#8)
* All media download types are supported

### select_ functions
* `galah_location` auto-detects the type of argument provided and so takes
a single argument, `query`, in place of `sf` and `wkt` (#17)
* `select_taxa` auto-detects the type of argument provided and so takes a single
argument, `query`, in place of `term` and `term_type` (#16)

### Minor improvements
* Provide more useful error message for empty occurrence download (#7)
* `ala_counts` uses the `group_by` field name as the returned `data.frame` column name (#6)
* `ala_occurrences` sends `sourceId` parameter to ALA (#5)
* `search_fields` provides a more helpful error for invalid types (#11)


# galah 1.0.0

First version of <code>galah</code>, built on earlier functionality from the <code>ALA4R</code> package.
