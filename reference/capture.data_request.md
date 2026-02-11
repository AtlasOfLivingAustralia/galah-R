# Capture a request

The first step in evaluating a request is to capture and parse the
information it contains. The resulting object has class `prequery` for
those requiring further processing or `query` for those that don't. A
`prequery` object shows the basic structure of what has been requested
by a user in a given
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).

## Usage

``` r
capture(x, ...)

# S3 method for class 'data_request'
capture(x, mint_doi = FALSE, ...)

# S3 method for class 'metadata_request'
capture(x, ...)

# S3 method for class 'files_request'
capture(x, thumbnail = FALSE, ...)

# S3 method for class 'list'
capture(x, ...)
```

## Arguments

- x:

  A `_request` object to convert to a `prequery`.

- ...:

  Other arguments, currently ignored

- mint_doi:

  Logical: should a DOI be minted for this download? Only applies to
  `type = "occurrences"` when atlas chosen is "ALA".

- thumbnail:

  Logical: should thumbnail-size images be returned? Defaults to
  `FALSE`, indicating full-size images are required.

## Value

Either an object of class `prequery` when further processing is
required; or `query` when it is not. Both classes are structurally
identical, being list-like and containing at the following slots:

- `type`: The type of query, serves as a lookup to the corresponding
  field in `show_all(apis)`

- `url`: Either:

  - a length-1 character giving the API to be queried; or

  - a `tibble()` containing at least the field `url` and optionally
    others

- `request`: captures the preceeding `_request` object (see
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md))

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

`capture()` →
[`compound()`](https://galah.ala.org.au/R/reference/compound.md) →
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
→
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
→
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md)

`capture()` is the first of the
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
workflow, and it parses the basic structure of a user request, returned
as a `prequery` object. A `prequery` object shows what has been
requested, before those calls are built by
[`compound()`](https://galah.ala.org.au/R/reference/compound.md) and
evaluated by
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md).
For simple cases, this gives the same result as running
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
while the `run_checks` argument of
[`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md)
is set to `FALSE`, but is slightly faster. In complex cases, it is
simply a precursor to
[`compound()`](https://galah.ala.org.au/R/reference/compound.md).

## See also

To open a piped query, see
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md).
For alternative operations on `_request` objects, see
[`compound()`](https://galah.ala.org.au/R/reference/compound.md),
[`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md),
[`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
or
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md).
