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
