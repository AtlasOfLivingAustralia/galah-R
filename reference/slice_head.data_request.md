# Subset rows using their positions

`slice()` lets you index rows by their (integer) locations. For objects
of classes `data_request` or `metadata_request`, only
[`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html) is
currently implemented, and selects the first `n` rows.

If `.data` has been grouped using
[`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md),
the operation will be performed on each group, so that (e.g.)
`slice_head(df, n = 5)` will select the first five rows in each group.

## Usage

``` r
# S3 method for class 'data_request'
slice_head(.data, ..., n, prop, by = NULL)

# S3 method for class 'metadata_request'
slice_head(.data, ..., n, prop, by = NULL)
```

## Arguments

- .data:

  An object of class `data_request`, created using
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)

- ...:

  Currently ignored

- n:

  The number of rows to be returned. If data are grouped
  [`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md),
  this operation will be performed on each group.

- prop:

  Currently ignored.

- by:

  Currently ignored.

## Value

An amended `data_request` with a completed `slice` slot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Limit number of rows returned to 3.
# In this case, our query returns the top 3 years with most records.
galah_call() |>
  identify("perameles") |>
  filter(year > 2010) |>
  group_by(year) |>
  count() |>
  slice_head(n = 3) |>
  collect()
} # }
```
