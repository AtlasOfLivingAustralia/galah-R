# Print galah objects

As of version 2.0, `galah` supports several bespoke object types.
Classes `data_request`, `metadata_request` and `files_request` are for
starting pipes to download different types of information. These objects
are parsed using
[`collapse()`](https://dplyr.tidyverse.org/reference/compute.html) into
a `query` object, which contains one or more URLs necessary to return
the requested information. This object is then passed to
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) and/or
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html).
Finally,
[`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md)
creates an object of class `galah_config` which (unsurprisingly) stores
configuration information.

## Usage

``` r
# S3 method for class 'data_request'
print(x, ...)

# S3 method for class 'files_request'
print(x, ...)

# S3 method for class 'metadata_request'
print(x, ...)

# S3 method for class 'query'
print(x, ...)

# S3 method for class 'prequery'
print(x, ...)

# S3 method for class 'computed_query'
print(x, ...)

# S3 method for class 'query_set'
print(x, ...)

# S3 method for class 'galah_config'
print(x, ...)
```

## Arguments

- x:

  an object of the appropriate `class`

- ...:

  Arguments to be passed to or from other methods

## Value

Print does not return an object; instead it prints a description of the
object to the console

## Examples

``` r
if (FALSE) { # \dontrun{
# The most common way to start a pipe is with `galah_call()`
# later functions update the `data_request` object
galah_call() |> # same as calling `request_data()`
  filter(year >= 2020) |>
  group_by(year) |>
  count()

# Metadata requests are formatted in a similar way
request_metadata() |>
  filter(field == basisOfRecord) |>
  unnest()

# Queries are converted into a `query_set` by `collapse()`
x <- galah_call() |> # same as calling `request_data()`
  filter(year >= 2020) |>
  count() |>
  collapse()
print(x)
  
# Each `query_set` contains one or more `query` objects
x[[3]]
} # }
```
