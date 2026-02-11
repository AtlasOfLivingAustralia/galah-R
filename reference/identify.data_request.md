# Narrow a query by passing taxonomic identifiers

When conducting a search or creating a data query, it is common to
identify a known taxon or group of taxa to narrow down the records or
results returned.
[`identify()`](https://rdrr.io/r/graphics/identify.html) is used to
identify taxa you want returned in a search or a data query. Users to
pass scientific names or taxonomic identifiers with pipes to provide
data only for the biological group of interest.

It is good to use
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
and
[`search_identifiers()`](https://galah.ala.org.au/R/reference/search_all.md)
first to check that the taxa you provide to `galah_identify()` return
the correct results.

## Usage

``` r
# S3 method for class 'data_request'
identify(x, ...)

# S3 method for class 'metadata_request'
identify(x, ...)

galah_identify(...)
```

## Arguments

- x:

  An object of class `data_request` or `metadata_request`.

- ...:

  One or more scientific names.

## Value

A `tibble` containing identified taxa.

## See also

[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
or [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
for other ways to filter a query. You can also use
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md) to
check that supplied names are being matched correctly on the
server-side; see
[taxonomic_searches](https://galah.ala.org.au/R/reference/taxonomic_searches.md)
for a detailed overview.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use `galah_identify()` to narrow your queries
galah_call() |> 
  identify("Eolophus") |>
  count() |>
  collect()

# If you know a valid taxon identifier, use `filter()` instead.
id <- "https://biodiversity.org.au/afd/taxa/009169a9-a916-40ee-866c-669ae0a21c5c"
galah_call() |> 
  filter(lsid == id)  |>
  count() |>
  collect()
} # }
```
