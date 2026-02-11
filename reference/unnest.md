# Unnest a query

This syntax is borrowed from `tidyr`, and is conceptually used in the
same way here, but in galah unnest amends the query to unnest
information server-side, rather than on your machine. It powers all of
the
[`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
functions in galah.

## Usage

``` r
unnest(.query)
```

## Arguments

- .query:

  An object of class `metadata_request`

## Value

An object of class `metadata_request`

## Details

Re-implementing existing functions has the consequence of supporting
consistent syntax with tidyverse, at the cost of potentially introducing
conflicts. This can be avoided by using the `::` operator where
required.

## Examples

``` r
if (FALSE) { # \dontrun{
# Return values of field `basisOfRecord`
request_metadata() |> 
  unnest() |> 
  filter(field == basisOfRecord) |> 
  collect()
  
# Using `galah::unnest()` in this way is equivalent to:
show_all(fields, "basisOfRecord") |> 
  show_values()
  
# to add information to a species list:
request_metadata() |>
  filter(list == "dr650") |>
  select(everything()) |>
  unnest() |>
  collect()
} # }
```
