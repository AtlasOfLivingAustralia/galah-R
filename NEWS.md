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
are made in quick succession via `select_filters()` or `ala_occurrences()` (#61,
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
* `select_filters()` negates assertion filters when required, fixing the issue
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
* `select_locations` auto-detects the type of argument provided and so takes
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
