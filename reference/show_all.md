# Show valid record information

The living atlases store a huge amount of information, above and beyond
the occurrence records that are their main output. In `galah`, one way
that users can investigate this information is by showing all the
available options or categories for the type of information they are
interested in. Functions prefixed with `show_all_` do this, displaying
all valid options for the information specified by the suffix.

`show_all()` is a helper function that can display multiple types of
information from `show_all_` sub-functions.

## Usage

``` r
show_all(..., limit = NULL, all_fields = FALSE)

show_all_apis(limit = NULL, all_fields = FALSE)

show_all_assertions(limit = NULL, all_fields = FALSE)

show_all_atlases(limit = NULL, all_fields = FALSE)

show_all_collections(limit = NULL, all_fields = FALSE)

show_all_config()

show_all_datasets(limit = NULL, all_fields = FALSE)

show_all_fields(limit = NULL, all_fields = FALSE)

show_all_licences(limit = NULL, all_fields = FALSE)

show_all_lists(limit = NULL, all_fields = FALSE)

show_all_profiles(limit = NULL, all_fields = FALSE)

show_all_providers(limit = NULL, all_fields = FALSE)

show_all_ranks(limit = NULL, all_fields = FALSE)

show_all_reasons(limit = NULL, all_fields = FALSE)
```

## Arguments

- ...:

  String showing what type of information is to be requested. See
  `Details` (below) for accepted values.

- limit:

  Optional number of values to return. Defaults to NULL, i.e. all
  records

- all_fields:

  **\[experimental\]** If `TRUE`,
  [`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
  also returns all columns available from the API, rather than the
  'default' columns traditionally provided via galah.

## Value

An object of class `tbl_df` and `data.frame` (aka a tibble) containing
all data of interest.

## Details

There are five categories of information, each with their own specific
sub-functions to look-up each type of information. The available types
of information for `show_all_` are:

|                |               |                                                                             |                          |
|----------------|---------------|-----------------------------------------------------------------------------|--------------------------|
| **Category**   | **Type**      | **Description**                                                             | **Sub-functions**        |
| Configuration  | `atlases`     | Show what atlases are available                                             | `show_all_atlases()`     |
|                | `apis`        | Show what APIs & functions are available for each atlas                     | `show_all_apis()`        |
|                | `config`      | Show information necessary for authentication                               | `show_all_config()`      |
|                | `reasons`     | Show what values are acceptable as 'download reasons' for a specified atlas | `show_all_reasons()`     |
| Data providers | `providers`   | Show which institutions have provided data                                  | `show_all_providers()`   |
|                | `collections` | Show the specific collections within those institutions                     | `show_all_collections()` |
|                | `datasets`    | Shows all the data groupings within those collections                       | `show_all_datasets()`    |
| Filters        | `assertions`  | Show results of data quality checks run by each atlas                       | `show_all_assertions()`  |
|                | `fields`      | Show fields that are stored in an atlas                                     | `show_all_fields()`      |
|                | `licenses`    | Show what copyright licenses are applied to media                           | `show_all_licenses()`    |
|                | `profiles`    | Show what data profiles are available                                       | `show_all_profiles()`    |
| Taxonomy       | `lists`       | Show what species lists are available                                       | `show_all_lists()`       |
|                | `ranks`       | Show valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.)               | `show_all_ranks()`       |

## References

- Darwin Core terms <https://dwc.tdwg.org/terms/>

## See also

Use the
[`search_all()`](https://galah.ala.org.au/R/reference/search_all.md)
function and `search_()` sub-functions to search for information. These
functions are used to pass valid arguments to
[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md),
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md),
and related functions.

## Examples

``` r
if (FALSE) { # \dontrun{
# See all supported atlases
show_all(atlases)

# Show a list of all available data quality profiles
show_all(profiles)

# Show a listing of all accepted reasons for downloading occurrence data
show_all(reasons)

# Show a listing of all taxonomic ranks
show_all(ranks)

# `show_all()` is synonymous with `request_metadata() |> collect()`
request_metadata(type = "fields") |>
  collect()
  
# using `all_fields = TRUE` is synonymous with `select(everything())`
request_metadata(type = "fields") |>
  select(everything()) |>
  collect()
} # }
```
