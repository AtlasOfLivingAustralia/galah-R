# Retrieve a database query

Retrieve the result of a query from the server. It is the default way to
end a piped query that begins with
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).

## Usage

``` r
# S3 method for class 'data_request'
collect(x, ..., wait = TRUE, file = NULL)

# S3 method for class 'metadata_request'
collect(x, ...)

# S3 method for class 'files_request'
collect(x, ...)

# S3 method for class 'prequery'
collect(x, ...)

# S3 method for class 'query'
collect(x, ...)

# S3 method for class 'query_set'
collect(x, ..., wait = TRUE, file = NULL)

# S3 method for class 'computed_query'
collect(x, ..., wait = TRUE, file = NULL)
```

## Arguments

- x:

  An object of class `data_request`, `metadata_request` or
  `files_request` (from
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md));
  or an object of class `prequery`, `query_set` or `query` (from
  [`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md),
  [`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  or
  [`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md))

- ...:

  Arguments passed on to other methods

- wait:

  logical; should `galah` wait for a response? Defaults to `FALSE`. Only
  applies for `type = "occurrences"` or `"species"`.

- file:

  (Optional) file name. If not given, will be set to `data` with date
  and time added. The file path (directory) is always given by
  `galah_config()$package$directory`.

## Value

In most cases,
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
returns a `tibble` containing requested data. Where the requested data
are not yet ready (i.e. for occurrences when `wait` is set to `FALSE`),
this function returns an object of class `query` that can be used to
recheck the download at a later time.

## Details

`galah` uses an object-based pipeline to convert piped requests into
valid queries, and to enact those queries with the specified
organisation. Typically, requests open with
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md) -
though
[`request_metadata()`](https://galah.ala.org.au/R/reference/galah_call.md)
and
[`request_files()`](https://galah.ala.org.au/R/reference/galah_call.md)
are also valid - and end with
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html). Under
the hood, the sequence of functions is as follows:

[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md)
→ [`compound()`](https://galah.ala.org.au/R/reference/compound.md) →
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
→
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
→ [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)

[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) is the
final step of the
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
workflow, and it retrieves the result of a query once it is processed by
the server.

## See also

To open a piped query, see
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).
For alternative operations on `_request` objects, see
[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md),
[`compound()`](https://galah.ala.org.au/R/reference/compound.md),
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
or
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md).
