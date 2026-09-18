# Set up authentication

Authenticate a request, either by sending a registered email address
(and, for GBIF, password and username); or by loggin in via the browser
to generate a JWT token. Note that while handling this manually in-pipe
is the most transparent approach, this function is also used to pass
cached information stored via
[`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md)
to a query. **\[experimental\]**.

## Usage

``` r
authenticate(
  .data,
  email = NULL,
  username = NULL,
  password = NULL,
  download_reason_id = NULL,
  use_jwt = FALSE,
  cache_jwt = FALSE,
  ...
)
```

## Arguments

- .data:

  An object of class `data_request` or `metadata_request`.

- email:

  (string) Email address registered with the selected organisation.

- username:

  (string) Registered username (GBIF only).

- password:

  (string) Registered password (GBIF only).

- download_reason_id:

  (integer) ID for the download reason. See
  [`show_all_reasons()`](https://galah.ala.org.au/R/reference/show_all.md)
  for accepted values.

- use_jwt:

  (logical) Should an OAuth workflow be used for authentication? Only
  supported for Flemish and Australian atlases. Defaults to `FALSE`

- cache_jwt:

  (logical) Should JWT tokens be cached to disk? Defaults to `FALSE`.

- ...:

  Other arguments, currently ignored.

## Value

An object of the same class as supplied, but with an added
`authenticate` slot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate occurrence queries within a pipe without using `galah_config()`
galah_call() |>
  authenticate(email = "your-email@email.com") |>
  identify("Wollemia nobilis") |>
  collect()

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
