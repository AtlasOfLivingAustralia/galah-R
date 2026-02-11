# Group by one or more variables

Most data operations are done on groups defined by variables.
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
takes a field name (unquoted) and performs a grouping operation. The
default behaviour is to use it in combination with
[`count()`](https://galah.ala.org.au/R/reference/count.data_request.md)
to give information on number of occurrences per level of that field.
Alternatively, you can use it without count to get a download of
occurrences grouped by that variable. This is particularly useful when
used with a taxonomic `ID` field (`speciesID`, `genusID` etc.) as it
allows further information to be appended to the result. This is how
[`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md)
works, for example. See
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md)
for details.

## Usage

``` r
# S3 method for class 'data_request'
group_by(.data, ...)

galah_group_by(...)
```

## Arguments

- .data:

  An object of class `data_request`

- ...:

  Zero or more individual column names to include

## Value

If any arguments are provided, returns a `data.frame` with columns
`name` and `type`, as per
[`select.data_request()`](https://galah.ala.org.au/R/reference/select.data_request.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# default usage is for grouping counts
galah_call() |> 
  group_by(basisOfRecord) |>
  counts() |>
  collect()

# Alternatively, we can use this with an occurrence search  
galah_call() |>
  filter(year == 2024,
         genus = "Crinia") |>
  group_by(speciesID) |>
 collect()
# note that this example is equivalent to `atlas_species()`; 
# but using `group_by()` is more flexible.
} # }
```
