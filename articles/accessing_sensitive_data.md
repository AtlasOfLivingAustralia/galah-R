# Accessing sensitive data

`galah` provides access to biodiversity information stored by GBIF and
its’ partner nodes. While much of this information is shared freely and
without restriction, there are a number of cases where this is not
appropriate, such as the locations of:

- threatened species, whose locations are protected by law in many
  jurisdictions
- species at risk from poachers, particularly for wildlife trade

This leads to a problem: data that is more open helps scientists and
policy-makers understand and conserve biodiversity, but also generates
threats to those species. To balance these concerns against one another,
it is common to ‘obfuscate’ sensitive data, which consists of providing
location information at a lower spatial resolution, making it harder to
locate threatened species on the ground.

## How ALA handles sensitive data

While many data providers solve this problem themselves in a range of
ways - such as by not sharing data at all, randomising some locations,
or reducing the number of decimal places of their locations - others
provide high-precision data on the understanding that it will only be
made available to specific users, and then only by written agreement. In
these cases, the ALA displays the obfuscated data publicly, but retains
the original data for use for specific purposes. Researchers can request
access to the original data via the National Framework for the Sharing
of Restricted Access Species Data in Australia, more simply known as the
‘RASD framework’ (<https://www.rasd.org.au>).

If your access to sensitive data is approved by the provider(s) in
question, you can use ‘galah’ to access that sensitive data.

## Using authentication

You have two choices for how to call authentication in your queries. The
simplest way is to ‘switch on’ authentication using
[`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md):

``` r
galah_config(authenticate = TRUE)
```

This is straightforward, but you’ll have to remember to switch
`authentication` to `FALSE` again once you’re done. A safer choice is to
use the
[`authenticate()`](https://galah.ala.org.au/R/reference/authenticate.md)
function in-pipe:

``` r
df <- galah_call() |>
  identify("Mammalia") |>
  filter(year == 2025) |>
  authenticate() |>
  collect()
```

It doesn’t matter where in the pipe you place
[`authenticate()`](https://galah.ala.org.au/R/reference/authenticate.md),
as it will only trigger your browser to open once you run
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html).
Whichever method you use, you’ll notice a page pop up in your default
browser, encouraging you to sign in. Once you have done so, you should
be redirected back to your chosen IDE to continue working.

## Deciding what sensitive data to access

In the the ALA, sensitive data are stored in bespoke fields, which have
an existing field name prefixed with `sensitive_`. They cannot be
requested directly. Instead, if you request a field that has a sensitive
counterpart, both the public and sensitive version of that field will be
returned.

| public_field        | restricted_field              |
|---------------------|-------------------------------|
| eventID             | sensitive_eventID             |
| eventDate           | sensitive_eventDate           |
| eventTime           | sensitive_eventTime           |
| month               | sensitive_month               |
| day                 | sensitive_day                 |
| locality            | sensitive_locality            |
| locationRemarks     | sensitive_locationRemarks     |
| decimalLatitude     | sensitive_decimalLatitude     |
| decimalLongitude    | sensitive_decimalLongitude    |
| footprintWKT        | sensitive_footprintWKT        |
| verbatimEventDate   | sensitive_verbatimEventDate   |
| verbatimLocality    | sensitive_verbatimLocality    |
| verbatimCoordinates | sensitive_verbatimCoordinates |
| verbatimLatitude    | sensitive_verbatimLatitude    |
| verbatimLongitude   | sensitive_verbatimLongitude   |

So to download data, we might try something like:

    result <- galah_call() |>
        filter(species_list_uid == "dr491") |>
        collect()

    > result
    # A tibble: 487 × 12
       recordID   scientificName taxonConceptID decimalLatitude sensitive_decimalLat…¹ decimalLongitude
       <chr>      <chr>          <chr>                    <dbl>                  <dbl>            <dbl>
     1 00825ab0-… Caladenia vul… https://id.bi…           -37.8                     NA             145.
     2 0094e7df-… Caladenia vul… https://id.bi…           -37.7                     NA             141.
     3 00d3a4e3-… Caladenia vul… https://id.bi…           -39.6                     NA             147.
     4 02305849-… Caladenia vul… https://id.bi…           -38.0                     NA             145.
     5 02e28dc1-… Caladenia vul… https://id.bi…           -37.7                     NA             145.
     6 068b4c68-… Caladenia vul… https://id.bi…           -37.7                     NA             141.
     7 07b5356b-… Caladenia vul… https://id.bi…           -37.0                     NA             143.
     8 0807fcbe-… Caladenia vul… https://id.bi…           -37.9                     NA             145.
     9 093df7f4-… Caladenia vul… https://id.bi…           -37.9                     NA             145.
    10 0942b709-… Caladenia vul… https://id.bi…           -38.4                     NA             145.
    # ℹ 477 more rows
    # ℹ abbreviated name: ¹​sensitive_decimalLatitude
    # ℹ 6 more variables: sensitive_decimalLongitude <dbl>, eventDate <dttm>,
    #   sensitive_eventDate <lgl>, basisOfRecord <chr>, occurrenceStatus <chr>, dataResourceName <chr>
    # ℹ Use `print(n = ...)` to see more rows

    > colnames(result)
     [1] "recordID"                   "scientificName"             "taxonConceptID"            
     [4] "decimalLatitude"            "sensitive_decimalLatitude"  "decimalLongitude"          
     [7] "sensitive_decimalLongitude" "eventDate"                  "sensitive_eventDate"       
    [10] "basisOfRecord"              "occurrenceStatus"           "dataResourceName" 
