# Collect media files

This function downloads full-sized or thumbnail images and media files
to a local directory using information from
[`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.md)

## Usage

``` r
collect_media(df, thumbnail = FALSE)
```

## Arguments

- df:

  A `tibble` returned by
  [`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.md) or a
  pipe starting with `request_data(type = "media")`.

- thumbnail:

  Default is `FALSE`. If `TRUE` will download small thumbnail-sized
  images, rather than full size images (default).

## Value

Invisibly returns a `tibble` listing the number of files downloaded,
grouped by their HTML status codes. Primarily called for the side effect
of downloading available image & media files to a user local directory.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use `atlas_media()` to return a `tibble` of records that contain media
x <- galah_call() |> 
  identify("perameles") |>
  filter(year == 2015) |>
  atlas_media()

# To download media files, add `collect_media()` to the end of a query 
galah_config(directory = "media_files")
collect_media(x)

# Since version 2.0, it is possible to run all steps in sequence
# first, get occurrences, making sure to include media fields:
occurrences_df <- request_data() |>
  identify("Regent Honeyeater") |>
  filter(!is.na(images), year == 2011) |>
  select(group = "media") |>
  collect()
 
# second, get media metadata
media_info <- request_metadata() |>
  filter(media == occurrences_df) |>
  collect()
  
# the two steps above + `right_join()` are synonymous with `atlas_media()`
# third, get images
request_files() |>
  filter(media == media_info) |>
  collect(thumbnail = TRUE)
# step three is synonymous with `collect_media()`
} # }
```
