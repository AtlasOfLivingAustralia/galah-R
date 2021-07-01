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
