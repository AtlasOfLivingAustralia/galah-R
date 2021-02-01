
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galah

[![Travis-CI Build
Status](https://travis-ci.com/AtlasOfLivingAustralia/galah.svg?branch=master)](https://travis-ci.com/AtlasOfLivingAustralia/galah)
[![codecov](https://codecov.io/gh/AtlasOfLivingAustralia/galah/branch/master/graph/badge.svg)](https://codecov.io/gh/AtlasOfLivingAustralia/galah)

`galah` is an R package for accessing data from the [Atlas of Living
Australia (ALA)](https://ala.org.au) APIs. It is the successor to
[`ALA4R`](https://github.com/AtlasOfLivingAustralia/ALA4R)

The ALA aggregates Australian biodiversity data from a range of sources,
and makes the data freely available for scientists, policy makers,
industry and the general public.

If you have any questions or suggestions, please [contact
us](mailto:support@ala.org.au).

## Citing the package

Please cite `galah`. To generate a citation for the package version you
are using, you can run:

``` r
citation(package = "galah")
```

## Citing data

If you’re using occurrence data downloaded through `galah` in a
publication, please generate a DOI and cite it. To generate a DOI, set
`mint_doi = TRUE` in a call to `ala_occurrences`. To generate a citation
using the dataset, use `ala_citation`

## Installing

`galah` is not yet available on CRAN. To install the package from
GitHub:

``` r
install.packages("remotes")
remotes::install_github("AtlasOfLivingAustralia/galah")
```

On Linux you will first need to ensure that `libcurl` and `v8` (version
&lt;= 3.15) are installed on your system — e.g. on Ubuntu/Debian, open a
terminal and do:

``` sh
sudo apt-get install libcurl4-openssl-dev libv8-3.14-dev
```

## Getting started

-   The `galah`
    [vignette](https://atlasoflivingaustralia.github.io/galah/articles/galah.html)
    provides an overview of the main features
-   Run `?galah` or view the [reference
    page](https://atlasoflivingaustralia.github.io/galah/reference/index.html)
    to view all functions.
