# Quick start guide

`galah` is an R interface to biodiversity data hosted by the Global
Biodiversity Information Facility ([GBIF](https://www.gbif.org)) and its
subsidiary node organisations. GBIF and its partner nodes collate and
store observations of individual life forms using the [‘Darwin
Core’](https://dwc.tdwg.org) data standard.

## Installation

To install from CRAN:

``` r
install.packages("galah")
```

Or install the development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("AtlasOfLivingAustralia/galah")
```

Load the package

``` r
library(galah)
```

## Configuration

Begin by choosing which organisation you would like `galah` to query,
and providing your registration information for that organisation.

``` r
galah_config(atlas = "GBIF",
             username = "user1",
             email = "email@email.com",
             password = "my_password")
```

The full list of supported queries by organisation is as follows:

![Fig 1: Organisations and APIs supported by
galah](../reference/figures/atlases_plot.png)

Fig 1: Organisations and APIs supported by galah

## Getting data

`galah` is a `dplyr` extension package; rather than using pipes to amend
a `tibble` in your workspace, you amend a query, which is then sent to
your chosen organisation. These pipes differ from traditional syntax in
two ways:

- they begin with a function - usually
  [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md) -
  instead of a `tibble`
- they end with one of `dplyr`’s evaluation functions, usually
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)

So an example query might be to find the number of records per year:

``` r
galah_config(atlas = "Australia")

galah_call() |>            # open a pipe
  filter(year >= 2020) |>  # choose rows to keep
  count(year) |>           # count the number of rows
  collect()                # retrieve query from the server
```

    ## # A tibble: 7 × 2
    ##   year     count
    ##   <chr>    <int>
    ## 1 2024  11889930
    ## 2 2023  11007491
    ## 3 2022   9430065
    ## 4 2025   9142677
    ## 5 2021   8695248
    ## 6 2020   7311836
    ## 7 2026    309836

Or to find the number of categories present in a dataset, for example
how many species are present:

``` r
galah_call() |>
  identify("Crinia") |>   # filters by taxonomic names
  distinct(speciesID) |>  # keep only unique values
  count() |>
  collect()
```

    ## # A tibble: 1 × 1
    ##   count
    ##   <int>
    ## 1    17

You can ‘glimpse’ a data download before you run it, to check all the
data you need is included:

``` r
galah_call() |>
  identify("Eolophus roseicapilla") |> 
  filter(year == 2010) |>
  glimpse() |>
  collect()
```

    ## Rows: 21,984
    ## Columns: 8
    ## $ taxonConceptID   <chr> "https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea", "https://biodiversity.org.au…
    ## $ eventDate        <dbl> 1.272672e+12, 1.289002e+12, 1.291014e+12
    ## $ scientificName   <chr> "Eolophus roseicapilla", "Eolophus roseicapilla", "Eolophus roseicapilla"
    ## $ decimalLatitude  <dbl> -25.98833, -37.83032, -35.41707
    ## $ decimalLongitude <dbl> 152.0442, 144.9812, 138.6868
    ## $ basisOfRecord    <chr> "HUMAN_OBSERVATION", "HUMAN_OBSERVATION", "HUMAN_OBSERVATION"
    ## $ dataResourceName <chr> "BirdLife Australia, Birdata", "eBird Australia", "eBird Australia"
    ## $ occurrenceStatus <chr> "PRESENT", "ABSENT", "ABSENT"

And, once satisfied that your parameters are correct, download the
records themselves:

``` r
galah_call() |>
  identify("Eolophus roseicapilla") |> 
  filter(year == 2010) |>
  select(eventDate, decimalLatitude, species) |>
  collect()
```

    ## # A tibble: 21,984 × 3
    ##    eventDate decimalLatitude species              
    ##    <dttm>              <dbl> <chr>                
    ##  1 NA                  -36.5 Eolophus roseicapilla
    ##  2 NA                  -38.2 Eolophus roseicapilla
    ##  3 NA                  -37.0 Eolophus roseicapilla
    ##  4 NA                  -37.7 Eolophus roseicapilla
    ##  5 NA                  -35.6 Eolophus roseicapilla
    ##  6 NA                  -31.1 Eolophus roseicapilla
    ##  7 NA                  -38.2 Eolophus roseicapilla
    ##  8 NA                  -38.2 Eolophus roseicapilla
    ##  9 NA                  -38.2 Eolophus roseicapilla
    ## 10 NA                  -38.2 Eolophus roseicapilla
    ## # ℹ 21,974 more rows

This works because many of the functions in `dplyr` are “generic”,
meaning it is possible to write extensions that apply them to new object
classes. In our case,
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)
creates a new object class called a `data_request` for which we have
written new extensions. This means that galah will not interfere with
your use of
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
friends on your tibbles. Supported `dplyr` verbs that modify queries are
as follows:

- [`arrange.data_request()`](https://galah.ala.org.au/R/reference/arrange.data_request.md)
- [`count.data_request()`](https://galah.ala.org.au/R/reference/count.data_request.md)
- [`distinct.data_request()`](https://galah.ala.org.au/R/reference/distinct.data_request.md)
- [`filter.data_request()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
- [`glimpse.data_request()`](https://galah.ala.org.au/R/reference/glimpse.data_request.md)
- [`group_by.data_request()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
- [`select.data_request()`](https://galah.ala.org.au/R/reference/select.data_request.md)
- [`slice_head.data_request()`](https://galah.ala.org.au/R/reference/slice_head.data_request.md)

Additional verbs are:

- [`apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md)
- [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md) or
  [`st_crop.data_request()`](https://galah.ala.org.au/R/reference/geolocate.md)
- [`identify.data_request()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
- [`unnest()`](https://galah.ala.org.au/R/reference/unnest.md)

It is good practice to download your data in as few steps as possible,
to minimize impacts on the server, and to ensure you can get a single
DOI for your data. See the [download data
reproducibly](https://galah.ala.org.au/R/articles/download-data-reproducibly.md)
vignette for details.

## Finding information

Building queries using
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) requires
that you know two things:

- what **fields** (columns) are present in the dataset you are searching
- what **values** exist for those fields

Finding this information requires looking for metadata:

``` r
request_metadata(type = "fields") |>
  collect()
```

    ## # A tibble: 639 × 3
    ##    id                  description               type  
    ##    <chr>               <chr>                     <chr> 
    ##  1 abcdTypeStatus      <NA>                      fields
    ##  2 acceptedNameUsage   Accepted name             fields
    ##  3 acceptedNameUsageID Accepted name             fields
    ##  4 accessRights        Access rights             fields
    ##  5 annotationsDoi      <NA>                      fields
    ##  6 annotationsUid      Referenced by publication fields
    ##  7 assertionUserId     Assertions by user        fields
    ##  8 assertions          Record issues             fields
    ##  9 assertionsCount     <NA>                      fields
    ## 10 associatedMedia     Associated Media          fields
    ## # ℹ 629 more rows

You can browser this tibble using
[`View()`](https://rdrr.io/r/utils/View.html) or search it using
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html). Once
you have found a field that you want to include in your query, you can
find values for that field using
[`unnest()`](https://galah.ala.org.au/R/reference/unnest.md):

``` r
request_metadata() |>
  filter(fields == "cl22") |>
  unnest() |>
  collect()
```

    ## # A tibble: 11 × 1
    ##    cl22                        
    ##    <chr>                       
    ##  1 New South Wales             
    ##  2 Victoria                    
    ##  3 Queensland                  
    ##  4 South Australia             
    ##  5 Western Australia           
    ##  6 Northern Territory          
    ##  7 Tasmania                    
    ##  8 Australian Capital Territory
    ##  9 Macquarie Island            
    ## 10 Coral Sea Islands           
    ## 11 Ashmore and Cartier Islands

Different types of metadata are available; see
[`?request_metadata`](https://galah.ala.org.au/R/reference/galah_call.md)
for a full list.

## Wrapper functions

While `dplyr` syntax is very flexible, there are cases where it is
easier to simply say the sort of data you want, rather than create a
database query to implement it. For this reason, several common use
cases have their own wrapper functions.

The `atlas_` family of functions act like
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html), but
enforce a particular type of data to be returned, such as record counts:

``` r
galah_call() |>
  filter(year == 2025) |>
  atlas_counts()   # note no need for a `count()` function
```

    ## # A tibble: 1 × 1
    ##     count
    ##     <int>
    ## 1 9142677

Or occurrences:

``` r
galah_call() |>
  identify("Eolophus roseicapilla") |>
  filter(year == 2000,
         cl22 == "Australian Capital Territory") |>
  atlas_occurrences() |>
  print(n = 6)
```

    ## # A tibble: 2,032 × 9
    ##   recordID         scientificName taxonConceptID decimalLatitude decimalLongitude eventDate           basisOfRecord occurrenceStatus
    ##   <chr>            <chr>          <chr>                    <dbl>            <dbl> <dttm>              <chr>         <chr>           
    ## 1 0026d29f-b6ab-4… Eolophus rose… https://biodi…           -35.4             149. 2000-08-07 00:00:00 HUMAN_OBSERV… PRESENT         
    ## 2 0062d446-007b-4… Eolophus rose… https://biodi…           -35.3             149. 2000-03-10 00:00:00 HUMAN_OBSERV… PRESENT         
    ## 3 00a62ee0-1e08-4… Eolophus rose… https://biodi…           -35.2             149. 2000-01-29 00:00:00 HUMAN_OBSERV… PRESENT         
    ## 4 00ab2f4d-326f-4… Eolophus rose… https://biodi…           -35.4             149. 2000-09-25 00:00:00 HUMAN_OBSERV… PRESENT         
    ## 5 00ae4631-ea59-4… Eolophus rose… https://biodi…           -35.3             149. 2000-02-12 00:00:00 HUMAN_OBSERV… PRESENT         
    ## 6 00b6c8ec-e7b9-4… Eolophus rose… https://biodi…           -35.2             149. 2000-02-05 00:00:00 HUMAN_OBSERV… PRESENT         
    ## # ℹ 2,026 more rows
    ## # ℹ 1 more variable: dataResourceName <chr>

[`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md)
replaces the need for
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
call, while
[`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.md) is a
shortcut to a complex workflow that incorporates both data and metadata
calls; see the [downloading images and
sounds](https://galah.ala.org.au/R/articles/downloading_images_and_sounds.md)
vignette for details.

Finally, metadata calls can be made more efficiently using the
[`show_all()`](https://galah.ala.org.au/R/reference/show_all.md) and
[`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
functions. These take the same arguments as the `type` argument in
[`request_metadata()`](https://galah.ala.org.au/R/reference/galah_call.md),
but use non-standard evaluation, so they don’t require quotes. They are
also evaluated immediately rather than lazily:

``` r
show_all(fields)
```

    ## # A tibble: 639 × 3
    ##    id                  description               type  
    ##    <chr>               <chr>                     <chr> 
    ##  1 abcdTypeStatus      <NA>                      fields
    ##  2 acceptedNameUsage   Accepted name             fields
    ##  3 acceptedNameUsageID Accepted name             fields
    ##  4 accessRights        Access rights             fields
    ##  5 annotationsDoi      <NA>                      fields
    ##  6 annotationsUid      Referenced by publication fields
    ##  7 assertionUserId     Assertions by user        fields
    ##  8 assertions          Record issues             fields
    ##  9 assertionsCount     <NA>                      fields
    ## 10 associatedMedia     Associated Media          fields
    ## # ℹ 629 more rows

You can check the [look up
information](https://galah.ala.org.au/R/articles/lookup_up_information.md)
vignette for further details.
