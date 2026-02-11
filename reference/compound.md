# Force evaluation of a database query

`compound()` shows the full set of queries required to properly evaluate
the user's request, run prior to
[`collapse()`](https://dplyr.tidyverse.org/reference/compute.html).

The number of total queries to send for a single data request is often
broader than the single query returned by
[`collapse()`](https://dplyr.tidyverse.org/reference/compute.html). If,
for example, the user's query includes a call to
[`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md),
then a taxonomic query is required to run *before* the 'final' query is
attempted. In relation to other functions that manipulate `_request`
objects, `compound()` is called within
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md),
and itself calls
[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md)
internally where required.

## Usage

``` r
compound(x, ...)

# S3 method for class 'data_request'
compound(x, mint_doi = FALSE, ...)

# S3 method for class 'metadata_request'
compound(x, ...)

# S3 method for class 'files_request'
compound(x, ...)

# S3 method for class 'prequery'
compound(x, mint_doi = FALSE, ...)

# S3 method for class 'query'
compound(x, ...)

# S3 method for class 'query_set'
compound(x, ...)
```

## Arguments

- x:

  An object to be compounded. Works for `data_request`,
  `metadata_request`, `file_request`, `query` or `prequery`.

- ...:

  Other arguments passed to
  [`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md).

- mint_doi:

  Logical: should a DOI be minted for this download? Only applies to
  `type = "occurrences"`, and only for supported atlases.

## Value

An object of class `query_set`, which is simply a list of all `query`
objects required to properly evaluate the specified request. Objects are
listed in the order in which they will be evaluated, meaning the query
that the user has actually requested will be placed last.

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
→ `compound()` →
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
→
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
→
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md)

`compound()` is the second of the
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
workflow, and it collates the complete list of queries required to send
in order to meet the user's data request, returned by
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md).

## See also

To open a piped query, see
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).
For alternative operations on `_request` objects, see
[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md),
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md),
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
or
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md).
