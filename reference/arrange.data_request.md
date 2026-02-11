# Order rows using column values

`arrange.data_request()` arranges rows of a query on the server side,
meaning that the query is constructed in such a way that information
will be arranged when the query is processed. This only has an effect
when used in combination with
[`count()`](https://galah.ala.org.au/R/reference/count.data_request.md)
and
[`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md).
The benefit of using
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) within
a [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
pipe is that it is sometimes beneficial to choose a non-default order
for data to be delivered in, particularly if
[`slice_head()`](https://galah.ala.org.au/R/reference/slice_head.data_request.md)
is also called.

## Usage

``` r
# S3 method for class 'data_request'
arrange(.data, ...)

# S3 method for class 'metadata_request'
arrange(.data, ...)
```

## Arguments

- .data:

  An object of class `data_request`

- ...:

  A variable to arrange the resulting tibble by. Should be one of the
  variables also listed in
  [`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md).

## Value

An amended `data_request` with a completed `arrange` slot.

## Examples

``` r
if (FALSE) { # \dontrun{

# Arrange grouped counts by ascending year
galah_call() |>
  identify("Crinia") |>
  filter(year >= 2020) |>
  group_by(year) |>
  arrange(year) |>
  count() |>
  collect()
  
# Arrange grouped counts by ascending record count
galah_call() |>
  identify("Crinia") |>
  filter(year >= 2020) |>
  group_by(year) |>
  arrange(count) |>
  count() |>
  collect()

# Arrange grouped counts by descending year
galah_call() |>
  identify("Crinia") |>
  filter(year >= 2020) |>
  group_by(year) |>
  arrange(desc(year)) |>
  count() |>
  collect()
} # }
```
