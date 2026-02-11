# Compute a query

Sends a request for information to a server. This is useful for requests
that run a server-side process, as it separates the submission of the
request from its retrieval.

Within galah,
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) is
generally hidden as it is one part of the overall process to complete a
`data_request`, `metadata_request` or `file_request`. However, calling
[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) at the
end of a
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
sends a request to be completed server-side (i.e., outside of R), and
the result can be returned in R by calling
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md)
at a later time. This can be preferable to calling
[`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md),
which prevents execution of new code until the server-side process is
complete.

## Usage

``` r
# S3 method for class 'data_request'
compute(x, ...)

# S3 method for class 'metadata_request'
compute(x, ...)

# S3 method for class 'files_request'
compute(x, ...)

# S3 method for class 'prequery'
compute(x, ...)

# S3 method for class 'query'
compute(x, ...)

# S3 method for class 'query_set'
compute(x, ...)
```

## Arguments

- x:

  An object of class `data_request`, `metadata_request` or
  `files_request` (i.e. constructed using a pipe) or `query` (i.e.
  constructed by
  [`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md))

- ...:

  Arguments passed on to other methods

## Value

An object of class `computed_query`, which is identical to class `query`
except for occurrence data, where it also contains information on the
status of the request.

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
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md).
Under the hood, the sequence of functions is as follows:

[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md)
→ [`compound()`](https://galah.ala.org.au/R/reference/compound.md) →
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
→ [`compute()`](https://dplyr.tidyverse.org/reference/compute.html) →
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md)

[`compute()`](https://dplyr.tidyverse.org/reference/compute.html) sends
a query to a server, which, once completed, can be retrieved using
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md).

## See also

To open a piped query, see
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).
For alternative operations on `_request` objects, see
[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md),
[`compound()`](https://galah.ala.org.au/R/reference/compound.md),
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md),
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md).
