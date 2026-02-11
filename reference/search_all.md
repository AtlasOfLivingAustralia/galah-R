# Search for record information

The living atlases store a huge amount of information, above and beyond
the occurrence records that are their main output. In `galah`, one way
that users can investigate this information is by searching for a
specific option or category for the type of information they are
interested in. Functions prefixed with `search_` do this, displaying any
matches to a search term within the valid options for the information
specified by the suffix.

**For more information about taxonomic searches using `search_taxa()`,
see**
[`?taxonomic_searches`](https://galah.ala.org.au/R/reference/taxonomic_searches.md).

`search_all()` is a helper function that can do searches for multiple
types of information, acting as a wrapper around many `search_`
sub-functions. See `Details` (below) for accepted values.

## Usage

``` r
search_all(type, query, all_fields = FALSE)

search_assertions(query, all_fields = FALSE)

search_apis(query, all_fields = FALSE)

search_atlases(query, all_fields = FALSE)

search_collections(query, all_fields = FALSE)

search_datasets(query, all_fields = FALSE)

search_fields(query, all_fields = FALSE)

search_identifiers(..., all_fields = FALSE)

search_licences(query, all_fields = FALSE)

search_lists(query, all_fields = FALSE)

search_media(query, all_fields = FALSE)

search_profiles(query, all_fields = FALSE)

search_providers(query, all_fields = FALSE)

search_ranks(query, all_fields = FALSE)

search_reasons(query, all_fields = FALSE)

search_taxa(..., all_fields = FALSE)
```

## Arguments

- type:

  A string to specify what type of parameters should be searched.

- query:

  A string specifying a search term. Searches are not case-sensitive.

- all_fields:

  **\[experimental\]** If `TRUE`,
  [`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
  also returns all columns available from the API, rather than the
  'default' columns traditionally provided via galah.

- ...:

  One or more objects accepted by the taxonomic lookup services. See
  [taxonomic_searches](https://galah.ala.org.au/R/reference/taxonomic_searches.md)
  for details

## Value

An object of class `tbl_df` and `data.frame` (aka a tibble) containing
all data that match the search query.

## Details

There are six categories of information, each with their own specific
sub-functions to look-up each type of information. The available types
of information for `search_all()` are:

|                |               |                                                                                   |                        |
|----------------|---------------|-----------------------------------------------------------------------------------|------------------------|
| **Category**   | **Type**      | **Description**                                                                   | **Sub-functions**      |
| configuration  | `atlases`     | Search for what atlases are available                                             | `search_atlases()`     |
|                | `apis`        | Search for what APIs & functions are available for each atlas                     | `search_apis()`        |
|                | `reasons`     | Search for what values are acceptable as 'download reasons' for a specified atlas | `search_reasons()`     |
| taxonomy       | `taxa`        | Search for one or more taxonomic names                                            | `search_taxa()`        |
|                | `identifiers` | Take a universal identifier and return taxonomic information                      | `search_identifiers()` |
|                | `ranks`       | Search for valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.)               | `search_ranks()`       |
| filters        | `fields`      | Search for fields that are stored in an atlas                                     | `search_fields()`      |
|                | `assertions`  | Search for results of data quality checks run by each atlas                       | `search_assertions()`  |
|                | `licenses`    | Search for copyright licences applied to media                                    | `search_licenses()`    |
| group filters  | `profiles`    | Search for what data profiles are available                                       | `search_profiles()`    |
|                | `lists`       | Search for what species lists are available                                       | `search_lists()`       |
| data providers | `providers`   | Search for which institutions have provided data                                  | `search_providers()`   |
|                | `collections` | Search for the specific collections within those institutions                     | `search_collections()` |
|                | `datasets`    | Search for the data groupings within those collections                            | `search_datasets()`    |
| media          | `media`       | Search for images or sounds using a vector of IDs                                 | `search_media()`       |

## See also

Use the [`show_all()`](https://galah.ala.org.au/R/reference/show_all.md)
function and `show_all_()` sub-functions to show available options of
information. These functions are used to pass valid arguments to
[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md),
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md),
and related functions. Taxonomic queries are somewhat more involved; see
[taxonomic_searches](https://galah.ala.org.au/R/reference/taxonomic_searches.md)
for details.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for fields that include the word "date"
search_all(fields, "date")

# Search for fields that include the word "marine"
search_all(fields, "marine")

# Search using a single taxonomic term
# (see `?search_taxa()` for more information)
search_all(taxa, "Reptilia") # equivalent

# Look up a unique taxon identifier
# (see `?search_identifiers()` for more information)
search_all(identifiers, 
           "https://id.biodiversity.org.au/node/apni/2914510")

# Search for species lists that match "endangered"
search_all(lists, "endangered") # equivalent

# Search for a valid taxonomic rank, "subphylum"
search_all(ranks, "subphylum")

# An alternative is to download the data and then `filter` it. This is 
# largely synonymous, and allows greater control over which fields are searched.
request_metadata(type = "fields") |>
 collect() |>
 dplyr::filter(grepl("date", id))
} # }
```
