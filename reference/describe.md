# Describe what fields are available

Unlike a local `tibble`, where printing to the console shows what fields
are available, when working remotely there is no way to know what fields
are present that can be queried. This function name is borrowed from
SQL, where it is used to give a read-out of fields that are in the
source database. **\[experimental\]**

## Usage

``` r
describe(x, ...)

# S3 method for class 'data_request'
describe(x, ...)
```

## Arguments

- x:

  An object of class `data_request`

- ...:

  Other arguments, currently ignored

## Value

A `tibble` showing the `id`, `description` and `data_type` for all
fields, or if
[`select()`](https://dplyr.tidyverse.org/reference/select.html) is
called, then those fields requested by the user. See
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md)
for other examples of valid `group` values.

## See also

[`glimpse()`](https://galah.ala.org.au/R/reference/glimpse.data_request.md)
for a different way to view results of a query;
[`show_all_fields()`](https://galah.ala.org.au/R/reference/show_all.md)
for full metadata on available fields;
[`distinct()`](https://galah.ala.org.au/R/reference/distinct.data_request.md)
for showing the values *within* a given field.

## Examples

``` r
if (FALSE) { # \dontrun{
# By default, this shows all fields in the source system
galah_call() |>
  describe() |>
  collect()

# If `select()` is called, only requested fields are shown
galah_call() |>
  select(group = "basic") |>
  describe() |>
  collect()
} # }
```
