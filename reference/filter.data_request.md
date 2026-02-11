# Keep rows that match a condition

The [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
function is used to subset a data, retaining all rows that satisfy your
conditions. To be retained, the row must produce a value of `TRUE` for
all conditions. Unlike 'local' filters that act on a `tibble`, the galah
implementations work by amending a query which is then enacted by
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) or one
of the `atlas_` family of functions (such as
[`atlas_counts()`](https://galah.ala.org.au/R/reference/atlas_.md) or
[`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md)).

## Usage

``` r
# S3 method for class 'data_request'
filter(.data, ...)

# S3 method for class 'metadata_request'
filter(.data, ...)

# S3 method for class 'files_request'
filter(.data, ...)

galah_filter(...)
```

## Arguments

- .data:

  An object of class `data_request`, `metadata_request` or
  `files_request`, created using
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
  or related functions.

- ...:

  Expressions that return a logical value, and are defined in terms of
  the variables in the selected atlas (and checked using
  `show_all(fields)`. If multiple expressions are included, they are
  combined with the & operator. Only rows for which all conditions
  evaluate to `TRUE` are kept.

## Value

A tibble containing filter values.

## Details

*Syntax*

[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) uses
non-standard evaluation (NSE), and is designed to be as compatible as
possible with
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
syntax. Permissible examples include:

- `==` (e.g. `year = 2020`) but not `=` (for consistency with `dplyr`)

- `!=`, e.g. `year != 2020`)

- `>` or `>=` (e.g. `year >= 2020`)

- `<` or `<=` (e.g. `year <= 2020`)

- `OR` statements (e.g. `year == 2018 | year == 2020`)

- `AND` statements (e.g. `year >= 2000 & year <= 2020`)

Some general tips:

- Separating statements with a comma is equivalent to an `AND`
  statement; Ergo `filter(year >= 2010 & year < 2020)` is the same as
  `_filter(year >= 2010, year < 2020)`.

- All statements must include the field name; so
  `filter(year == 2010 | year == 2021)` works, as does
  `filter(year == c(2010, 2021))`, but `filter(year == 2010 | 2021)`
  fails.

- It is possible to use an object to specify required values, e.g.
  `year_value <- 2010; filter(year > year_value)`.

- `solr` supports range queries on text as well as numbers; so
  `filter(cl22 >= "Tasmania")` is valid.

- It is possible to filter by 'assertions', which are statements about
  data validity, such as
  `filter(assertions != c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")`.
  Valid assertions can be found using `show_all(assertions)`.

*Exceptions*

When querying occurrences, species, or their respective counts (i.e. all
of the above examples), field names are checked internally against
`show_all(fields)`. There are some cases where bespoke field names are
required, as follows.

When requesting a data download from a DOI, the field `doi` is valid,
i.e.:

    galah_call() |>
      filter(doi = "a-long-doi-string") |>
      collect()

For taxonomic metadata, the `taxa` field is valid:

    request_metadata() |>
      filter(taxa == "Chordata") |>
      unnest()

For building taxonomic trees, the `rank` field is valid:

    request_data() |>
      identify("Chordata") |>
      filter(rank == "class") |>
      atlas_taxonomy()

Media queries are more involved, but break two rules: they accept the
`media` field, and they accept a tibble on the rhs of the equation. For
example, users wishing to break down media queries into their respective
API calls should begin with an occurrence query:

    occurrences <- galah_call() |>
       identify("Litoria peronii) |>
       select(group = c("basic", "media") |>
       collect()

They can then use the `media` field to request media metadata:

    media_metadata <- request_metadata |>
      filter(media == occurrences) |>
      collect()

And finally, the metadata tibble can be used to request files:

    request_files() |>
      filter(media == media_metadata) |>
      collect()

## See also

[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md),
[`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
and [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
for other ways to amend the information returned by
[`atlas_()`](https://galah.ala.org.au/R/reference/atlas_.md) functions.
Use `search_all(fields)` to find fields that you can filter by, and
[`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
to find what values of those filters are available.

## Examples

``` r
if (FALSE) { # \dontrun{
# basic example
galah_call() |>
  filter(year >= 2019,
         basisOfRecord == "HumanObservation") |>
  count() |>
  collect()

# Field names can be parsed from objects using `{{}}` syntax, e.g. 
field <- "year"
value <- "2025"
galah_call() |> 
  filter({{field}} == value) |>
  count() |>
  collect()
} # }
```
