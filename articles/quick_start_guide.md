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

So an example query might be to find the number of records:

``` r

galah_call() |>    # open a pipe
  count() |>       # count the number of rows
  collect()        # retrieve query from the server
```

    ## # A tibble: 1 × 1
    ##       count
    ##       <int>
    ## 1 184796858

By default, queries are sent to the Atlas of Living Australia, but this
can be changed using the `from` argument:

``` r

galah_call(from = "Spain") |>
  count() |>
  collect()
```

    ## # A tibble: 1 × 1
    ##      count
    ##      <int>
    ## 1 59852882

The `from` argument accepts the `acronym`, `region`, or `institution`
fields from `show_all(atlases)`. The full list of supported queries by
organisation is as follows:

![Figure 1: Organisations (rows) and APIs (columns) supported by
galah](../reference/figures/atlases_plot.png)

Figure 1: Organisations (rows) and APIs (columns) supported by galah

## Filtering

You can pass taxonomic filters to your query with
[`identify()`](https://rdrr.io/r/graphics/identify.html):

``` r

galah_call() |>
  identify("Eolophus roseicapilla") |>
  count() |>
  collect()
```

    ## # A tibble: 1 × 1
    ##     count
    ##     <int>
    ## 1 1593848

To choose records using non-taxonomic criteria, you’ll need to use
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html). This
reqires that you find out:

- what **fields** (columns) are present in the dataset you are searching
- what **values** exist for those fields

You can answer the first question with
[`describe()`](https://galah.ala.org.au/R/reference/describe.md):

``` r

galah_call() |>
  describe() |>
  collect()
```

    ## # A tibble: 582 × 3
    ##    id                    description               data_type
    ##    <chr>                 <chr>                     <chr>    
    ##  1 acceptedNameUsage     Accepted name             string   
    ##  2 acceptedNameUsageID   Accepted name             string   
    ##  3 accessRights          Access rights             string   
    ##  4 annotationsDoi        <NA>                      string   
    ##  5 annotationsUid        Referenced by publication string   
    ##  6 assertionUserId       Assertions by user        string   
    ##  7 assertions            Record issues             string   
    ##  8 assertionsCount       <NA>                      int      
    ##  9 associatedMedia       Associated Media          string   
    ## 10 associatedOccurrences Associated Occurrences    string   
    ## # ℹ 572 more rows

Once you find a variable you are interested in, see what values it
contains using
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html):

``` r

galah_call() |>
  distinct(basisOfRecord) |>
  collect()
```

    ## # A tibble: 9 × 1
    ##   basisOfRecord      
    ##   <chr>              
    ## 1 HUMAN_OBSERVATION  
    ## 2 PRESERVED_SPECIMEN 
    ## 3 OCCURRENCE         
    ## 4 OBSERVATION        
    ## 5 MACHINE_OBSERVATION
    ## 6 MATERIAL_SAMPLE    
    ## 7 LIVING_SPECIMEN    
    ## 8 FOSSIL_SPECIMEN    
    ## 9 MATERIAL_CITATION

Then you can combine these within a
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) query:

``` r

galah_call() |>
  filter(basisOfRecord == "PRESERVED_SPECIMEN") |>
  count() |> 
  collect()
```

    ## # A tibble: 1 × 1
    ##      count
    ##      <int>
    ## 1 18419249

These functions can be combined in a number of ways, for example to
learn how many species from the genus ‘Crinia’ have records from each
country with records in the ALA:

``` r

galah_call() |>
  identify("Crinia") |>   # filters by taxonomic names
  group_by(country) |>    # grouping variable
  distinct(species) |>    # keep only unique values
  count() |>
  collect()
```

    ## # A tibble: 5 × 2
    ##   country          count
    ##   <chr>            <int>
    ## 1 Australia           17
    ## 2 Papua New Guinea     2
    ## 3 New Zealand          1
    ## 4 Indonesia            0
    ## 5 Solomon Islands      1

You can [`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) a
data download before you run it, to check all the data you need is
included:

``` r

galah_call() |>
  identify("Eolophus roseicapilla") |> 
  filter(year == 2010) |>
  glimpse() |>
  collect()
```

    ## Rows: 26,430
    ## Columns: 8
    ## $ taxonConceptID   <chr> "https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea", "https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea", "https://biodiversity.org.au/afd/taxa/a38ac5ac-3049-4fde-8391-faf8bf7aeb7f"
    ## $ eventDate        <dbl> 1.278833e+12, 1.286170e+12, 1.292803e+12
    ## $ scientificName   <chr> "Eolophus roseicapilla", "Eolophus roseicapilla", "Eolophus roseicapilla albiceps"
    ## $ decimalLatitude  <dbl> -27.57463, -16.06593, -35.88444
    ## $ decimalLongitude <dbl> 153.4350, 136.3067, 145.9089
    ## $ basisOfRecord    <chr> "HUMAN_OBSERVATION", "HUMAN_OBSERVATION", "HUMAN_OBSERVATION"
    ## $ dataResourceName <chr> "eBird Australia", "eBird Australia", "BirdLife Australia, Birdata"
    ## $ otherProperties  <list> ["PRESENT"], ["PRESENT"], ["PRESENT"]

And, once satisfied that your parameters are correct, download the
records themselves:

``` r

galah_call() |>
  authenticate(email = "registered_email@wherever.com") |>
  identify("Eolophus roseicapilla") |> 
  filter(year == 2010) |>
  select(eventDate, decimalLatitude, species) |>
  collect()
```

    ## Request for 26430 occurrences placed in queue.

    ## Current queue length: 1

    ## --

    ## 
    ℹ Downloading

    ## 
    ✔ Downloading [253ms]

    ## # A tibble: 26,430 × 3
    ##    eventDate decimalLatitude species              
    ##    <dttm>              <dbl> <chr>                
    ##  1 NA                  -37.1 Eolophus roseicapilla
    ##  2 NA                  -37.0 Eolophus roseicapilla
    ##  3 NA                  -37.1 Eolophus roseicapilla
    ##  4 NA                  -37.1 Eolophus roseicapilla
    ##  5 NA                  -37.2 Eolophus roseicapilla
    ##  6 NA                  -37.0 Eolophus roseicapilla
    ##  7 NA                  -37.0 Eolophus roseicapilla
    ##  8 NA                  -37.0 Eolophus roseicapilla
    ##  9 NA                  -37.0 Eolophus roseicapilla
    ## 10 NA                  -37.1 Eolophus roseicapilla
    ## # ℹ 26,420 more rows

## Architecture

This works because many of the functions in `dplyr` are “generic”,
meaning it is possible to write extensions that apply them to new object
classes. In our case,
[`request_data()`](https://galah.ala.org.au/R/reference/galah_call.md)
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
- [`describe()`](https://galah.ala.org.au/R/reference/describe.md)
- [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md) or
  [`st_crop.data_request()`](https://galah.ala.org.au/R/reference/geolocate.md)
- [`identify.data_request()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
- [`unnest()`](https://galah.ala.org.au/R/reference/unnest.md)

It is good practice to download your data in as few steps as possible,
to minimize impacts on the server, and to ensure you can get a single
DOI for your data. See the [download data
reproducibly](https://galah.ala.org.au/R/articles/download-data-reproducibly.md)
vignette for details.

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
    ##      count
    ##      <int>
    ## 1 10781991

Or occurrences:

``` r

galah_call() |>
  identify("Eolophus roseicapilla") |>
  filter(year == 2000,
         cl22 == "Australian Capital Territory") |>
  atlas_occurrences() |>
  print(n = 6)
```

    ## -----

    ## # A tibble: 2,318 × 9
    ##   recordID                             scientificName        taxonConceptID                                                            decimalLatitude decimalLongitude eventDate           basisOfRecord     occurrenceStatus dataResourceName   
    ##   <chr>                                <chr>                 <chr>                                                                               <dbl>            <dbl> <dttm>              <chr>             <chr>            <chr>              
    ## 1 0026d29f-b6ab-4a1d-9c57-6ee12cfde3a0 Eolophus roseicapilla https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea           -35.4             149. 2000-08-07 00:00:00 HUMAN_OBSERVATION PRESENT          Garden Bird Surveys
    ## 2 00a62ee0-1e08-4114-b0d8-9b7905472d53 Eolophus roseicapilla https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea           -35.2             149. 2000-01-29 00:00:00 HUMAN_OBSERVATION PRESENT          Garden Bird Surveys
    ## 3 00ab2f4d-326f-4b01-9a8a-1a10c1f77e3c Eolophus roseicapilla https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea           -35.4             149. 2000-09-25 00:00:00 HUMAN_OBSERVATION PRESENT          Garden Bird Surveys
    ## 4 00b6c8ec-e7b9-4d9f-9638-d962b1b4acfa Eolophus roseicapilla https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea           -35.2             149. 2000-02-05 00:00:00 HUMAN_OBSERVATION PRESENT          Garden Bird Surveys
    ## 5 00e36517-6518-42d5-8b69-90c526095fef Eolophus roseicapilla https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea           -35.4             149. 2000-01-01 00:00:00 HUMAN_OBSERVATION PRESENT          Garden Bird Surveys
    ## 6 010b463f-d34f-45ec-8733-91f676a299d8 Eolophus roseicapilla https://biodiversity.org.au/afd/taxa/9b4ad548-8bb3-486a-ab0a-905506c463ea           -35.2             149. 2000-03-12 00:00:00 HUMAN_OBSERVATION PRESENT          Garden Bird Surveys
    ## # ℹ 2,312 more rows

[`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md)
replaces the need for
[`distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
call, while
[`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.md) is a
shortcut to a more complex workflow that incorporates both data and
metadata calls. Finally, metadata calls can be made more efficiently
using the
[`show_all()`](https://galah.ala.org.au/R/reference/show_all.md) and
[`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
functions. These take the same arguments as the `type` argument in
[`request_metadata()`](https://galah.ala.org.au/R/reference/galah_call.md),
but us non-standard evaluation, so they don’t require quotes. They are
also evaluated immediately rather than lazily:

``` r

show_all(fields)
```

    ## # A tibble: 582 × 3
    ##    id                    description               type  
    ##    <chr>                 <chr>                     <chr> 
    ##  1 acceptedNameUsage     Accepted name             fields
    ##  2 acceptedNameUsageID   Accepted name             fields
    ##  3 accessRights          Access rights             fields
    ##  4 annotationsDoi        <NA>                      fields
    ##  5 annotationsUid        Referenced by publication fields
    ##  6 assertionUserId       Assertions by user        fields
    ##  7 assertions            Record issues             fields
    ##  8 assertionsCount       <NA>                      fields
    ##  9 associatedMedia       Associated Media          fields
    ## 10 associatedOccurrences Associated Occurrences    fields
    ## # ℹ 572 more rows

You can check the [look up
information](https://galah.ala.org.au/R/articles/look_up_information.html)
vignette for further details.
