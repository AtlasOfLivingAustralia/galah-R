# Apply a data quality profile

A 'profile' is a group of filters that are pre-applied by the ALA. Using
a data profile allows a query to be filtered quickly to the most
relevant or quality-assured data that is fit-for-purpose. For example,
the "ALA" profile is designed to exclude lower quality records, whereas
other profiles apply filters specific to species distribution modelling
(e.g. CDSM).

Note that only one profile can be loaded at a time; if multiple profiles
are given, the first valid profile is used.

For more bespoke editing of filters within a profile, use
[`filter.data_request()`](https://galah.ala.org.au/R/reference/filter.data_request.md).

## Usage

``` r
apply_profile(.data, ...)

galah_apply_profile(...)
```

## Arguments

- .data:

  An object of class `data_request`

- ...:

  a profile name. Should be a `string` - the name or abbreviation of a
  data quality profile to apply to the query. Valid values can be seen
  using `show_all(profiles)`

## Value

An updated `data_request` with a completed `apply_profile` slot.

## See also

[`show_all()`](https://galah.ala.org.au/R/reference/show_all.md) and
[`search_all()`](https://galah.ala.org.au/R/reference/search_all.md) to
look up available data profiles.
[`filter.data_request()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
can be used for more bespoke editing of individual data profile filters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Apply a data quality profile to a query
galah_call() |> 
  identify("reptilia") |>
  filter(year == 2021) |>
  apply_profile(ALA) |>
  atlas_counts()
} # }
```
