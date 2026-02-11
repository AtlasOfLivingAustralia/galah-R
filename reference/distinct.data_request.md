# Keep distinct/unique rows

Keep only unique/distinct rows from a data frame. This is similar to
[`unique.data.frame()`](https://rdrr.io/r/base/unique.html) but
considerably faster. It is evaluated lazily.

## Usage

``` r
# S3 method for class 'data_request'
distinct(.data, ..., .keep_all = FALSE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See Methods, below, for more
  details.

- ...:

  Variables to use when determining uniqueness. Unlike the `dplyr`
  implementation this must be set for the function to do anything, and
  only a single variable is used.

- .keep_all:

  If `TRUE`, keep all variables in .data. Defaults to `FALSE`

## Details

This function has several potential uses. In it's default mode, it
simply shows the unique values for a supplied field:

    galah_call() |>
      distinct(basisOfRecord) |>
      collect()

    # A tibble: 9 × 1
      basisOfRecord
      <chr>
    1 HUMAN_OBSERVATION
    2 PRESERVED_SPECIMEN
    3 OCCURRENCE
    4 MACHINE_OBSERVATION
    5 OBSERVATION
    6 MATERIAL_SAMPLE
    7 LIVING_SPECIMEN
    8 FOSSIL_SPECIMEN
    9 MATERIAL_CITATION

This is the same result as you would get using
[`show_values()`](https://galah.ala.org.au/R/reference/show_values.md):

    search_all(fields, "basisOfRecord") |>
      show_values()

Using
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) is
somewhat more reliable, however, as it doesn't rely on searching the
tibble returned by `show_all(fields)`. It is also more efficient,
particularly when caching is turned off. If the goal is to retrieve the
*number* of levels of a factor, use:

    galah_call() |>
      distinct(basisOfRecord) |>
      count() |>
      collect()

    # A tibble: 1 × 1
      count
      <int>
    1     9

When the variable passed to
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html) in
the above example is `speciesID`, this is identical to calling:

    atlas_counts(type = "species")

You can also pass
[`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
to find the number of facets per level of a second variable:

    galah_call() |>
      identify("Perameles") |>
      distinct(speciesID) |>
      group_by(basisOfRecord) |>
      count() |>
      collect()

    # A tibble: 8 × 2
      basisOfRecord       count
      <chr>               <int>
    1 Human observation       7
    2 Preserved specimen      9
    3 Machine observation     2
    4 Observation             3
    5 Occurrence              3
    6 Material Sample         4
    7 Fossil specimen         1
    8 Living specimen         1

By setting `.keep_all = TRUE`, we get more information on each record.
Due to limits on the APIs this is not a perfect analogy for running
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
on raw occurrences; but it does allow us to generalise
[`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md) to
use any taxonomic identifier. For example, we might choose to show data
by family instead of species:

    galah_call() |>
      identify("Coleoptera") |>
      distinct(familyID, .keep_all = TRUE) |>
      collect()

Using
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) is
also valid:

    galah_call() |>
        filter(year == 2024,
               genus == "Crinia") |>
        group_by(speciesID) |>
        distinct(.keep_all = TRUE) |>
        collapse()

In this case,
[`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md)
and [`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md)
are synonymous, with the exception that the latter does not require you
to set the `.keep_all` argument to `TRUE`. So you could instead use:

    galah_call() |>
      identify("Coleoptera") |>
      distinct(familyID) |>
      atlas_species()

## Examples

``` r
if (FALSE) { # \dontrun{
galah_call() |>
  distinct(basisOfRecord) |>
  count() |>
  collect()
} # }
```
