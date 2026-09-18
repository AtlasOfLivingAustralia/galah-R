# Superseded functions

These functions are still valid, but have been superseded by more recent
versions. They were designed to be used to supply arguments within
functions from the
[atlas\_](https://galah.ala.org.au/R/reference/atlas_.md) family.
Instead you should use piped functions for this same functionality.

## Usage

``` r
galah_apply_profile(...)

galah_filter(...)

galah_geolocate(..., type = c("polygon", "bbox", "radius"))

galah_polygon(...)

galah_bbox(...)

galah_radius(...)

galah_group_by(...)

galah_identify(...)

galah_select(..., group = NULL)
```

## Arguments

- ...:

  Arguments passed to the function in question, usually (but not always)
  using non-standard evaluation.

- type:

  `string`: one of `"polygon"`, `"bbox"` or `"radius"`. Defaults to
  `"polygon"`. If `type = "polygon"`, a multipolygon will be built via
  [`geolocate_polygon()`](https://galah.ala.org.au/R/reference/geolocate.md).
  If `type = "bbox"`, a multipolygon will be built via
  [`geolocate_bbox()`](https://galah.ala.org.au/R/reference/geolocate.md).
  The multipolygon is used to narrow a query to the ALA.

- group:

  `string`: (optional) name of one or more column groups to include.
  Valid options are `"basic"`, `"event"` `"taxonomy"`, `"media"` and
  `"assertions"`.

## Details

Replacements are as follows:

- [`apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md)
  instead of `galah_apply_profile()`

- [`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
  instead of `galah_filter()`

- [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
  instead of `galah_geolocate()`

- [`geolocate_polygon()`](https://galah.ala.org.au/R/reference/geolocate.md)
  instead of `galah_polygon()`

- [`geolocate_bbox()`](https://galah.ala.org.au/R/reference/geolocate.md)
  instead of `galah_bbox()`

- [`geolocate_radius()`](https://galah.ala.org.au/R/reference/geolocate.md)
  instead of `galah_radius()`

- [`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
  instead of `galah_group_by()`

- [`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
  instead of `galah_identify()`

- [`select()`](https://galah.ala.org.au/R/reference/select.data_request.md)
  instead of `galah_select()`
