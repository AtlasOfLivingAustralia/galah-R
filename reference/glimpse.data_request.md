# Get a glimpse of your data

[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) is like a
transposed version of [`print()`](https://rdrr.io/r/base/print.html):
columns run down the page, and data runs across. This makes it possible
to see every column in a data frame. It's a little like
[`str()`](https://rdrr.io/r/utils/str.html) applied to a data frame but
it tries to show you as much data as possible. This implementation is
specific to `galah` and is evaluated lazily. **\[experimental\]**

## Usage

``` r
# S3 method for class 'data_request'
glimpse(x, ...)

# S3 method for class 'occurrences_glimpse'
print(x, ...)
```

## Arguments

- x:

  An object of class `data_request`

- ...:

  Other arguments, currently ignored

## Details

This implementation of
[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) actually
involves changing the API call sent to the server, then returning a
novel object class with it's own
[`print()`](https://rdrr.io/r/base/print.html) method.

## Examples

``` r
if (FALSE) { # \dontrun{
galah_call() |>
  filter(year >= 2019,
         basisOfRecord == "HumanObservation") |>
  select(year, basisOfRecord, species) |>
  glimpse() |>
  collect()
} # }
```
