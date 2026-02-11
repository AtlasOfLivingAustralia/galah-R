# Show or search for values within a specified field

Users may wish to see the specific values *within* a chosen field,
profile or list to narrow queries or understand more about the
information of interest. `show_values()` provides users with these
values. `search_values()` allows users for search for specific values
within a specified field.

## Usage

``` r
show_values(df, all_fields = FALSE)

search_values(df, query)
```

## Arguments

- df:

  A search result from
  [`search_fields()`](https://galah.ala.org.au/R/reference/search_all.md),
  [`search_profiles()`](https://galah.ala.org.au/R/reference/search_all.md)
  or
  [`search_lists()`](https://galah.ala.org.au/R/reference/search_all.md).

- all_fields:

  **\[experimental\]** If `TRUE`, `show_values()` also returns all
  columns available from the API, rather than the 'default' columns
  traditionally provided via galah.

  For lists, this will include 'raw' columns; columns included prior to
  the dataset's ingestion into the ALA, and will often include raw
  scientific names and vernacular names. For conservation lists like the
  EPBC list, this also includes columns containing each species'
  conservation status information.

  For other forms of metadata, setting this to `TRUE` may return more
  information than you want or need. Default is set to `FALSE`.

- query:

  A string specifying a search term. Not case sensitive.

## Value

A `tibble` of values for a specified field, profile or list.

## Details

Each **Field** contains categorical or numeric values. For example:

- The `field` "year" contains values 2021, 2020, 2019, etc.

- The `field` "stateProvince" contains values New South Wales, Victoria,
  Queensland, etc. These are used to narrow queries with
  [`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md).

Each **Profile** consists of many individual quality filters. For
example, the "ALA" profile consists of values:

- Exclude all records where spatial validity is FALSE

- Exclude all records with a latitude value of zero

- Exclude all records with a longitude value of zero

Each **List** contains a list of species, usually by taxonomic name. For
example, the Endangered Plant species list contains values:

- Acacia curranii (Curly-bark Wattle)

- Brachyscome papillosa (Mossgiel Daisy)

- Solanum karsense (Menindee Nightshade)

## Examples

``` r
if (FALSE) { # \dontrun{
# Show values in field 'cl22'
search_fields("cl22") |> 
  show_values()

# This is synonymous with `request_metadata() |> unnest()`.
# For example, the previous example can be run using:
request_metadata() |>
  filter(field == "cl22") |>
  unnest() |>
  collect() 

# Search for any values in field 'cl22' that match 'tas'
search_fields("cl22") |> 
  search_values("tas")

# See items within species list "dr19257"
search_lists("dr19257") |> 
  show_values()
} # }
```
