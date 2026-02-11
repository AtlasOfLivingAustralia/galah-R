# Start building a request

To download data from the selected atlas, one must construct a query.
This query tells the atlas API what data to download and return, as well
as how it should be filtered. Using `galah_call()` allows you to build a
piped query to download data, in the same way that you would wrangle
data with `dplyr` and the `tidyverse`. It is synonymous with
`request_data()`; to query other data types call `request_metadata()` or
`request_files()`.

## Usage

``` r
galah_call(
  type = c("occurrences", "occurrences-count", "occurrences-doi", "species",
    "species-count")
)

request_data(
  type = c("occurrences", "occurrences-count", "occurrences-doi", "species",
    "species-count")
)

request_metadata(
  type = c("fields", "apis", "assertions", "atlases", "collections", "config",
    "datasets", "licences", "lists", "media", "profiles", "providers", "ranks",
    "reasons", "taxa", "identifiers")
)

request_files(type = "media")
```

## Arguments

- type:

  string: what form of data should be returned? Acceptable values are
  specified by the corresponding `request` function

## Value

Each sub-function returns a different object class:

- `request_data()` and `galah_call()` return class `"data_request"`

- `request_metadata()` returns class `"metadata_request"`

- `request_files()` returns class `"files_request"`

These objects are list-like and store later dplyr verbs in the order
they are provided.

## Details

`galah_call()` and any of the `request_` functions are used to begin a
piped query, which is then actioned using
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md),
or optionally one of the
[`atlas_`](https://galah.ala.org.au/R/reference/atlas_.md) family of
functions.

Having distinct functions for different types of request is useful
because it allows `galah` to separate different types of requests to
perform better. For example,
[`filter.data_request()`](https://galah.ala.org.au/R/reference/filter.data_request.md)translates
filters to `solr` syntax for the living atlases, or to predicates for
GBIF, whereas
[`filter.metadata_request()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
adds a search term to your metadata query.

## See also

To amend a request object, use
[`apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md),
[`arrange()`](https://galah.ala.org.au/R/reference/arrange.data_request.md),
[`count()`](https://galah.ala.org.au/R/reference/count.data_request.md),
[`distinct()`](https://galah.ala.org.au/R/reference/distinct.data_request.md),
[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md),
[`glimpse()`](https://galah.ala.org.au/R/reference/glimpse.data_request.md),
[`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md),
[`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md),
[`select`](https://galah.ala.org.au/R/reference/select.data_request.md),
[`slice_head()`](https://galah.ala.org.au/R/reference/slice_head.data_request.md)
or [`unnest()`](https://galah.ala.org.au/R/reference/unnest.md). For
operations on `_request` objects, see
[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md),
[`compound()`](https://galah.ala.org.au/R/reference/compound.md),
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md),
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
or
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md).

## Examples

``` r
if (FALSE) { # \dontrun{ 
# Begin your query with `galah_call()`, then pipe using `%>%` or `|>`

# Get number of records of *Aves* from 2001 to 2004 by year
galah_call() |>
  identify("Aves") |>
  filter(year > 2000 & year < 2005) |>
  group_by(year) |>
  count() |>
  collect()
  
# Get information for all species in *Cacatuidae* family
galah_call() |>
  identify("Cacatuidae") |>
  distinct("speciesID", .keep_all = TRUE) |>
  collect()
  
# Download records of genus *Eolophus* from 2001 to 2004
galah_config(email = "your-email@email.com")

galah_call() |>
  identify("Eolophus") |>
  filter(year > 2000 & year < 2005) |>
  collect()
} # }
```
