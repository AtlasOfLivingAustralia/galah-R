# Set up authentication

Add an authentication slot to a query. That slot is then used by later
code to determine whether to add an OAuth workflow. It is triggered
automatically within
[`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md)
if the `authenticate` argument of
[`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md)
is set to `TRUE`, but only for occurrence queries to the Atlas of Living
Australia. **\[experimental\]**.

## Usage

``` r
authenticate(.data, cache_disk = FALSE)
```

## Arguments

- .data:

  An object of class `data_request` or `metadata_request`

- cache_disk:

  (logical) Should JWT tokens be cached to disk? Defaults to `FALSE`

## Value

An object of the same class as supplied, but with an added
`authenticate` slot.

## Examples

``` r
if (FALSE) { # \dontrun{
# use `galah_config()` to set for all occurrence queries
galah_config(authenticate = TRUE)

x <- galah_call() |>
  identify("Wollemia nobilis") |>
  collect()

# use in-pipe for more control
x <- galah_call() |>
  identify("Wollemia nobilis") |>
  authenticate() |>
  collect()
} # }
```
