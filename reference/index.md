# Package index

## Getting started

- [`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md)
  : View or set package behaviour
- [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
  [`request_data()`](https://galah.ala.org.au/R/reference/galah_call.md)
  [`request_metadata()`](https://galah.ala.org.au/R/reference/galah_call.md)
  [`request_files()`](https://galah.ala.org.au/R/reference/galah_call.md)
  : Start building a request

## Update a request object

- [`apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md)
  [`galah_apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md)
  : Apply a data quality profile
- [`arrange(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/arrange.data_request.md)
  [`arrange(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/arrange.data_request.md)
  : Order rows using column values
- [`authenticate()`](https://galah.ala.org.au/R/reference/authenticate.md)
  **\[experimental\]** : Set up authentication
- [`count(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/count.data_request.md)
  [`add_count(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/count.data_request.md)
  : Count the observations in each group
- [`distinct(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/distinct.data_request.md)
  : Keep distinct/unique rows
- [`filter(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/filter.data_request.md)
  [`filter(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/filter.data_request.md)
  [`filter(`*`<files_request>`*`)`](https://galah.ala.org.au/R/reference/filter.data_request.md)
  [`galah_filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
  : Keep rows that match a condition
- [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
  [`galah_geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
  [`galah_polygon()`](https://galah.ala.org.au/R/reference/geolocate.md)
  [`galah_bbox()`](https://galah.ala.org.au/R/reference/geolocate.md)
  [`galah_radius()`](https://galah.ala.org.au/R/reference/geolocate.md)
  [`st_crop(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/geolocate.md)
  : Narrow a query to within a specified area
- [`glimpse(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/glimpse.data_request.md)
  [`print(`*`<occurrences_glimpse>`*`)`](https://galah.ala.org.au/R/reference/glimpse.data_request.md)
  **\[experimental\]** : Get a glimpse of your data
- [`group_by(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
  [`galah_group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
  : Group by one or more variables
- [`identify(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/identify.data_request.md)
  [`identify(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/identify.data_request.md)
  [`galah_identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
  : Narrow a query by passing taxonomic identifiers
- [`select(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/select.data_request.md)
  [`select(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/select.data_request.md)
  [`galah_select()`](https://galah.ala.org.au/R/reference/select.data_request.md)
  : Keep or drop columns using their names
- [`slice_head(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/slice_head.data_request.md)
  [`slice_head(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/slice_head.data_request.md)
  : Subset rows using their positions
- [`reexports`](https://galah.ala.org.au/R/reference/reexports.md)
  [`add_count`](https://galah.ala.org.au/R/reference/reexports.md)
  [`arrange`](https://galah.ala.org.au/R/reference/reexports.md)
  [`collect`](https://galah.ala.org.au/R/reference/reexports.md)
  [`compute`](https://galah.ala.org.au/R/reference/reexports.md)
  [`collapse`](https://galah.ala.org.au/R/reference/reexports.md)
  [`count`](https://galah.ala.org.au/R/reference/reexports.md)
  [`distinct`](https://galah.ala.org.au/R/reference/reexports.md)
  [`filter`](https://galah.ala.org.au/R/reference/reexports.md)
  [`glimpse`](https://galah.ala.org.au/R/reference/reexports.md)
  [`group_by`](https://galah.ala.org.au/R/reference/reexports.md)
  [`select`](https://galah.ala.org.au/R/reference/reexports.md)
  [`slice_head`](https://galah.ala.org.au/R/reference/reexports.md)
  [`identify`](https://galah.ala.org.au/R/reference/reexports.md)
  [`st_crop`](https://galah.ala.org.au/R/reference/reexports.md) :
  Objects exported from other packages
- [`unnest()`](https://galah.ala.org.au/R/reference/unnest.md) : Unnest
  a query

## Create and execute a query

- [`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md)
  : Capture a request
- [`compound()`](https://galah.ala.org.au/R/reference/compound.md) :
  Force evaluation of a database query
- [`collapse(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  [`collapse(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  [`collapse(`*`<files_request>`*`)`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  [`collapse(`*`<prequery>`*`)`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  [`collapse(`*`<query>`*`)`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  [`collapse(`*`<query_set>`*`)`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  : Generate a query
- [`compute(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  [`compute(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  [`compute(`*`<files_request>`*`)`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  [`compute(`*`<prequery>`*`)`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  [`compute(`*`<query>`*`)`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  [`compute(`*`<query_set>`*`)`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  : Compute a query
- [`collect(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  [`collect(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  [`collect(`*`<files_request>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  [`collect(`*`<prequery>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  [`collect(`*`<query>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  [`collect(`*`<query_set>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  [`collect(`*`<computed_query>`*`)`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  : Retrieve a database query

## Wrappers for accessing data

- [`show_all()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_apis()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_assertions()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_atlases()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_collections()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_config()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_datasets()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_fields()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_licences()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_lists()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_profiles()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_providers()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_ranks()`](https://galah.ala.org.au/R/reference/show_all.md)
  [`show_all_reasons()`](https://galah.ala.org.au/R/reference/show_all.md)
  : Show valid record information
- [`search_all()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_assertions()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_apis()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_atlases()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_collections()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_datasets()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_fields()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_identifiers()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_licences()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_lists()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_media()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_profiles()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_providers()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_ranks()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_reasons()`](https://galah.ala.org.au/R/reference/search_all.md)
  [`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
  : Search for record information
- [`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
  [`search_values()`](https://galah.ala.org.au/R/reference/show_values.md)
  : Show or search for values within a specified field
- [`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md)
  [`atlas_counts()`](https://galah.ala.org.au/R/reference/atlas_.md)
  [`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md)
  [`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.md)
  [`atlas_taxonomy()`](https://galah.ala.org.au/R/reference/atlas_.md) :
  Retrieve a database query
- [`collect_media()`](https://galah.ala.org.au/R/reference/collect_media.md)
  : Collect media files

## Miscellaneous functions

- [`taxonomic_searches`](https://galah.ala.org.au/R/reference/taxonomic_searches.md)
  : Look up taxon information

- [`read_zip()`](https://galah.ala.org.au/R/reference/read_zip.md) :
  Read downloaded data from a zip file

- [`atlas_citation()`](https://galah.ala.org.au/R/reference/atlas_citation.md)
  : Generate a citation for occurrence data

- [`as_data_filter()`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`as_predicates_filter()`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`as_metadata_filter()`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`as_files_filter()`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`print(`*`<data_filter>`*`)`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`print(`*`<predicates_filter>`*`)`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`print(`*`<metadata_filter>`*`)`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  [`print(`*`<files_filter>`*`)`](https://galah.ala.org.au/R/reference/filter_object_classes.md)
  :

  Object classes for
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  queries

- [`print(`*`<data_request>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<files_request>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<metadata_request>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<query>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<prequery>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<computed_query>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<query_set>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  [`print(`*`<galah_config>`*`)`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  : Print galah objects
