# Read downloaded data from a zip file

Living atlases supply data downloads as zip files. This function reads
these data efficiently, i.e. without unzipping them first, using the
`readr` package. Although this function has been part of galah for some
time, it was previously internal to
[`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md).
It has been exported now to support easy re-importing of downloaded
files, without the need to re-run a query.

## Usage

``` r
read_zip(file)
```

## Arguments

- file:

  (character) A file name. Must be a length-1 character ending in
  `.zip`.

## Examples

``` r
if (FALSE) { # \dontrun{
# set a working directory
galah_config(directory = "data-raw",
             email = "an-email-address@email.com")

# download some data
galah_call() |>
  identify("Heleioporus") |>
  filter(year == 2022) |>
  collect(file = "burrowing_frog_data.zip")
  
# load data from file
x <- read_zip("./data-raw/burrowing_frog_data.zip")
} # }
```
