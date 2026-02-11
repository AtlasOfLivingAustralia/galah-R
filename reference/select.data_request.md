# Keep or drop columns using their names

Select (and optionally rename) variables in a data frame, using a
concise mini-language that makes it easy to refer to variables based on
their name. Note that unlike calling
[`select()`](https://dplyr.tidyverse.org/reference/select.html) on a
local tibble, this implementation is only evaluated at the
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
stage, meaning any errors or messages will be triggered at the end of
the pipe.

[`select()`](https://dplyr.tidyverse.org/reference/select.html) supports
`dplyr` **selection helpers**, including:

- [`everything`](https://tidyselect.r-lib.org/reference/everything.html):
  Matches all variables. This is treated unusually in `galah`; see
  `details`.

- [`last_col`](https://tidyselect.r-lib.org/reference/everything.html):
  Select last variable, possibly with an offset.

Other helpers select variables by matching patterns in their names:

- [`starts_with`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Starts with a prefix.

- [`ends_with`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Ends with a suffix.

- [`contains`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Contains a literal string.

- [`matches`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Matches a regular expression.

- [`num_range`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Matches a numerical range like x01, x02, x03.

Or from variables stored in a character vector:

- [`all_of`](https://tidyselect.r-lib.org/reference/all_of.html):
  Matches variable names in a character vector. All names must be
  present, otherwise an out-of-bounds error is thrown.

- [`any_of`](https://tidyselect.r-lib.org/reference/all_of.html): Same
  as `all_of()`, except that no error is thrown for names that don't
  exist.

Or using a predicate function:

- [`where`](https://tidyselect.r-lib.org/reference/where.html): Applies
  a function to all variables and selects those for which the function
  returns `TRUE`.

## Usage

``` r
# S3 method for class 'data_request'
select(.data, ..., group = NULL)

# S3 method for class 'metadata_request'
select(.data, ...)

galah_select(..., group = NULL)
```

## Arguments

- .data:

  An object of class `data_request`, created using
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).

- ...:

  Zero or more individual column names to include.

- group:

  `string`: (optional) name of one or more column groups to include.
  Valid options are `"basic"`, `"event"` `"taxonomy"`, `"media"` and
  `"assertions"`.

## Value

A tibble specifying the name and type of each column to include in the
call to
[`atlas_counts()`](https://galah.ala.org.au/R/reference/atlas_.md) or
[`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md).

## Details

GBIF nodes store content in hundreds of different fields, and users
often require thousands or millions of records at a time. To reduce time
taken to download data, and limit complexity of the resulting `tibble`,
it is sensible to restrict the fields returned by occurrence queries.
The full list of available fields can be viewed with `show_all(fields)`.
Note that
[`select()`](https://dplyr.tidyverse.org/reference/select.html) and
`galah_select()` are supported for all atlases that allow downloads,
with the exception of GBIF, for which all columns are returned.

Calling the argument `group = "basic"` returns the following columns:

- `recordID`

- `scientificName`

- `taxonConceptID`

- `decimalLatitude`

- `decimalLongitude`

- `eventDate`

- `basisOfRecord`

- `occurrenceStatus`

- `dataResourceName`

Using `group = "event"` returns the following columns:

- `eventRemarks`

- `eventTime`

- `eventID`

- `eventDate`

- `samplingEffort`

- `samplingProtocol`

Using `group = "media"` returns the following columns:

- `multimedia`

- `multimediaLicence`

- `images`

- `videos`

- `sounds`

Using `group = "taxonomy"` returns higher taxonomic information for a
given query. It is the only `group` that is accepted by
[`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md) as
well as
[`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md).

Using `group = "assertions"` returns all quality assertion-related
columns. The list of assertions is shown by
[`show_all_assertions()`](https://galah.ala.org.au/R/reference/show_all.md).

For
[`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md),
arguments passed to `...` should be valid field names, which you can
check using `show_all(fields)`. For
[`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md), it
should be one or more of:

- `counts` to include counts of occurrences per species.

- `synonyms` to include any synonymous names.

- `lists` to include authoritative lists that each species is included
  on.

For metadata queries - as generated using
[`request_metadata()`](https://galah.ala.org.au/R/reference/galah_call.md)
or
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md) -
[`select()`](https://dplyr.tidyverse.org/reference/select.html) can now
be used to return only the requested columns. Unlike data queries, this
works by capturing the user's query and applying it user-side, rather
than amending the query.

## See also

[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md),
[`st_crop()`](https://galah.ala.org.au/R/reference/geolocate.md) and
[`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
for other ways to restrict the information returned; `show_all(fields)`
to list available fields.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download occurrence records of *Perameles*, 
# Only return scientificName and eventDate columns
galah_config(email = "your-email@email.com")
galah_call() |>
  identify("perameles")|>
  select(scientificName, eventDate) |>
  collect()

# Only return the "basic" group of columns and the basisOfRecord column
galah_call() |>
  identify("perameles") |>
  select(basisOfRecord, group = "basic") |>
  collect()
  
# When used in a pipe, `galah_select()` and `select()` are synonymous.
# Hence the previous example can be rewritten as:
galah_call() |>
  galah_identify("perameles") |>
  galah_select(basisOfRecord, group = "basic") |>
  collect()
} # }
```
