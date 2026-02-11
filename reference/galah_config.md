# View or set package behaviour

The `galah` package supports queries to a number of different data
providers, and once selected, it is desirable that all later queries are
sent to that organisation. Rather than supply this information
separately in each query, it is more parsimonious to cache it centrally
and call it as needed, which is what this function supports.

Beyond choosing an organisation, there are several other use cases for
caching. Many GBIF nodes require the user to supply a registered email
address, password, and (in some cases) a reason for downloading data,
all stored via `galah_config()`. This function also provides a
convenient place to control optional package behaviours, such as
checking queries to ensure they are valid (`run_checks`), informing you
via email when your downloads are ready (`send_email`), or controlling
whether galah will provide updates on your query as they are processed
(`verbose`).

## Usage

``` r
galah_config(...)
```

## Arguments

- ...:

  Options can be defined using the form `name = "value"`, or as a
  (single) named list. See details for accepted fields.

## Value

Returns an object with classes `galah_config` and `list`, invisibly if
arguments are supplied.

## Details

Valid arguments to this function are:

- `atlas` string: Living Atlas to point to, Australia by default. Can be
  an organisation name, acronym, or region (see
  [`show_all_atlases()`](https://galah.ala.org.au/R/reference/show_all.md)
  for admissible values)

- `authenticate` logical: Should `galah` use authenticate your queries
  using JWT tokens? Defaults to `FALSE`. If `TRUE`, user credentials are
  verified prior to sending a query. This can allow users with special
  access to download additional information in `galah`.

- `caching` logical: should metadata query results be cached in
  [`options()`](https://rdrr.io/r/base/options.html)? Defaults to `TRUE`
  for improved stability and speed.

- `directory` string: The directory to use for the disk cache. By
  default this is a temporary directory, which means that results will
  only be cached within an R session and cleared automatically when the
  user exits R. The user may wish to set this to a non-temporary
  directory for caching across sessions. The directory must exist on the
  file system.

- `download_reason_id` numeric or string: the "download reason"
  required. by some ALA services, either as a numeric ID (currently
  0–13) or a string (see `show_all(reasons)` for a list of valid ID
  codes and names). By default this is NA. Some ALA services require a
  valid download_reason_id code, either specified here or directly to
  the associated R function.

- `email` string: An email address that has been registered with the
  chosen atlas. For the ALA, you can register at [this
  address](https://auth.ala.org.au/userdetails/registration/createAccount).

- `password` string: A registered password (GBIF only)

- `run_checks` logical: should `galah` run checks for filters and
  columns. If making lots of requests sequentially, checks can slow down
  the process and lead to HTTP 500 errors, so should be turned off.
  Defaults to TRUE.

- `send_email` logical: should you receive an email for each query to
  [`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md)?
  Defaults to `FALSE`; but can be useful in some instances, for example
  for tracking DOIs assigned to specific downloads for later citation.

- `username` string: A registered username (GBIF only)

- `verbose` logical: should `galah` give verbose such as progress bars?
  Defaults to `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# To download occurrence records, enter your email in `galah_config()`. 
# This email should be registered with the atlas in question. 
galah_config(email = "your-email@email.com")
 
# Turn on caching in your session
galah_config(caching = TRUE)
 
# Some ALA services require that you add a reason for downloading data. 
# Add your selected reason using the option `download_reason_id`
galah_config(download_reason_id = 0)

# To look up all valid reasons to enter, use `show_all(reasons)`
show_all(reasons)

# Make debugging in your session easier by setting `verbose = TRUE`
galah_config(verbose = TRUE)

# Optionally supply arguments via a named list
list(email = "your-email@email.com") |>
  galah_config()
} # }
```
