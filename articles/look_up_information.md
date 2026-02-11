# Look up information

galah supports two functions to look up information:
[`show_all()`](https://galah.ala.org.au/R/reference/show_all.md) and
[`search_all()`](https://galah.ala.org.au/R/reference/search_all.md).
The first argument to both functions is a type of information that you
wish to look up; for example to see what fields are available to filter
a query by, use:

``` r
show_all(fields)
```

    ## # A tibble: 695 × 3
    ##    id                  description               type  
    ##    <chr>               <chr>                     <chr> 
    ##  1 _nest_parent_       <NA>                      fields
    ##  2 _nest_path_         <NA>                      fields
    ##  3 _root_              <NA>                      fields
    ##  4 abcdTypeStatus      <NA>                      fields
    ##  5 acceptedNameUsage   Accepted name             fields
    ##  6 acceptedNameUsageID Accepted name             fields
    ##  7 accessRights        Access rights             fields
    ##  8 annotationsDoi      <NA>                      fields
    ##  9 annotationsUid      Referenced by publication fields
    ## 10 assertionUserId     Assertions by user        fields
    ## # ℹ 685 more rows

And to search for a specific field:

``` r
search_all(fields, "australian states")
```

    ## # A tibble: 2 × 3
    ##   id     description                            type  
    ##   <chr>  <chr>                                  <chr> 
    ## 1 cl2013 ASGS Australian States and Territories fields
    ## 2 cl22   Australian States and Territories      fields

Here is a list of information types that can be used with
[`show_all()`](https://galah.ala.org.au/R/reference/show_all.md) and
[`search_all()`](https://galah.ala.org.au/R/reference/search_all.md):

| Information type   | Description                                                                 | Sub-functions                                |
|--------------------|-----------------------------------------------------------------------------|----------------------------------------------|
| **Configuration**  |                                                                             |                                              |
| atlases            | Show what living atlases are available                                      | show_all_atlases(), search_atlases()         |
| apis               | Show what APIs & functions are available for each atlas                     | show_all_apis(), search_apis()               |
| reasons            | Show what values are acceptable as ‘download reasons’ for a specified atlas | show_all_reasons(), search_reasons()         |
| **Taxonomy**       |                                                                             |                                              |
| taxa               | Search for one or more taxonomic names                                      | search_taxa()                                |
| identifiers        | Take a universal identifier and return taxonomic information                | search_identifiers()                         |
| ranks              | Show valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.)               | show_all_ranks(), search_ranks())            |
| **Filters**        |                                                                             |                                              |
| fields             | Show fields that are stored in an atlas                                     | show_all_fields(), search_fields()           |
| assertions         | Show results of data quality checks run by each atlas                       | show_all_assertions(), search_assertions()   |
| **Group filters**  |                                                                             |                                              |
| profiles           | Show what data quality profiles are available                               | show_all_profiles(), search_profiles()       |
| lists              | Show what species lists are available                                       | show_lists(), search_lists()                 |
| **Data providers** |                                                                             |                                              |
| providers          | Show which institutions have provided data                                  | show_all_providers(), search_providers()     |
| collections        | Show the specific collections within those institutions                     | show_all_collections(), search_collections() |
| datasets           | Shows all the data groupings within those collections                       | show_all_datasets(), search_datasets()       |

## `show_all_` subfunctions

While `show_all` is useful for a variety of cases, you can still call
the underlying subfunctions if you prefer. Functions with the prefix
`show_all_` do exactly that; they show all the possible values of the
category specified.

``` r
show_all_atlases()
```

    ## # A tibble: 10 × 4
    ##    region         institution                                                             acronym url                    
    ##    <chr>          <chr>                                                                   <chr>   <chr>                  
    ##  1 Australia      Atlas of Living Australia                                               ALA     https://www.ala.org.au 
    ##  2 Austria        Biodiversitäts-Atlas Österreich                                         BAO     https://biodiversityat…
    ##  3 Brazil         Sistemas de Informações sobre a Biodiversidade Brasileira               SiBBr   https://sibbr.gov.br   
    ##  4 France         Portail français d'accès aux données d'observation sur les espèces      OpenObs https://openobs.mnhn.fr
    ##  5 Global         Global Biodiversity Information Facility                                GBIF    https://gbif.org       
    ##  6 Guatemala      Sistema Nacional de Información sobre Diversidad Biológica de Guatemala SNIBgt  https://snib.conap.gob…
    ##  7 Portugal       GBIF Portugal                                                           GBIF.pt https://www.gbif.pt    
    ##  8 Spain          GBIF Spain                                                              GBIF.es https://gbif.es        
    ##  9 Sweden         Swedish Biodiversity Data Infrastructure                                SBDI    https://biodiversityda…
    ## 10 United Kingdom National Biodiversity Network                                           NBN     https://nbn.org.uk

``` r
show_all_reasons()
```

    ## # A tibble: 13 × 2
    ##       id name                            
    ##    <int> <chr>                           
    ##  1     1 biosecurity management/planning 
    ##  2    11 citizen science                 
    ##  3     5 collection management           
    ##  4     0 conservation management/planning
    ##  5     7 ecological research             
    ##  6     3 education                       
    ##  7     2 environmental assessment        
    ##  8    12 restoration/remediation         
    ##  9     4 scientific research             
    ## 10     8 systematic research/taxonomy    
    ## 11    13 species modelling               
    ## 12     6 other                           
    ## 13    10 testing

## `search_` subfunctions

You can also call subfunctions that use the `search_` prefix to lookup
information. `search_` subfunctions differ from `show_all_` in that they
require a query to work, and they useful to search for detailed
information that can’t be summarised across the whole atlas.

[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md) is
an especially useful function in galah. It let’s you search for a single
taxon or multiple taxa by name.

``` r
search_taxa("reptilia")
```

    ## # A tibble: 1 × 9
    ##   search_term scientific_name taxon_concept_id                               rank  match_type kingdom phylum class issues
    ##   <chr>       <chr>           <chr>                                          <chr> <chr>      <chr>   <chr>  <chr> <chr> 
    ## 1 reptilia    REPTILIA        https://biodiversity.org.au/afd/taxa/682e1228… class exactMatch Animal… Chord… Rept… noIss…

``` r
search_taxa("reptilia", "aves", "mammalia", "pisces")
```

    ## # A tibble: 1 × 9
    ##   search_term scientific_name taxon_concept_id                               rank  match_type kingdom phylum class issues
    ##   <chr>       <chr>           <chr>                                          <chr> <chr>      <chr>   <chr>  <chr> <chr> 
    ## 1 reptilia    REPTILIA        https://biodiversity.org.au/afd/taxa/682e1228… class exactMatch Animal… Chord… Rept… noIss…

Alternatively,
[`search_identifiers()`](https://galah.ala.org.au/R/reference/search_all.md)
is the partner function to
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md).
If we already know a taxonomic identifier, we can search for which taxa
the identifier belongs to.

``` r
search_identifiers("urn:lsid:biodiversity.org.au:afd.taxon:682e1228-5b3c-45ff-833b-550efd40c399")
```

    ## # A tibble: 1 × 15
    ##   search_term     success scientific_name taxon_concept_id rank  rank_id   lft   rgt match_type kingdom kingdom_id phylum
    ##   <chr>           <lgl>   <chr>           <chr>            <chr>   <int> <int> <int> <chr>      <chr>   <chr>      <chr> 
    ## 1 urn:lsid:biodi… TRUE    REPTILIA        https://biodive… class    3000 33626 36658 taxonIdMa… Animal… https://b… Chord…
    ## # ℹ 3 more variables: phylum_id <chr>, class <chr>, class_id <chr>

## `show_values()` & `search_values()`

Once a desired field is found, you can use
[`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
to understand the information contained within that field. For example,
we can show the values contained in the field `basisOfRecord`.

``` r
search_all(fields, "basisOfRecord") |> show_values()
```

    ## ! Search returned 2 matched fields.
    ## • Showing values for 'basisOfRecord'.

    ## # A tibble: 9 × 1
    ##   basisOfRecord      
    ##   <chr>              
    ## 1 HUMAN_OBSERVATION  
    ## 2 PRESERVED_SPECIMEN 
    ## 3 MACHINE_OBSERVATION
    ## 4 OCCURRENCE         
    ## 5 OBSERVATION        
    ## 6 MATERIAL_SAMPLE    
    ## 7 LIVING_SPECIMEN    
    ## 8 MATERIAL_CITATION  
    ## 9 FOSSIL_SPECIMEN

Use this information to pass meaningful queries to
[`galah_filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md).

``` r
galah_call() |> 
  galah_filter(basisOfRecord == "LIVING_SPECIMEN") |> 
  atlas_counts()
```

    ## # A tibble: 1 × 1
    ##    count
    ##    <int>
    ## 1 126135

This works for other types of query, such as data profiles:

``` r
search_all(profiles, "ALA") |> 
  show_values() |> 
  head()
```

    ## • Showing values for 'ALA'.

    ## # A tibble: 6 × 5
    ##      id enabled description                                                                           filter displayOrder
    ##   <int> <lgl>   <chr>                                                                                 <chr>         <int>
    ## 1    94 TRUE    "Exclude all records where spatial validity is \"false\""                             "-spa…            1
    ## 2    96 TRUE    "Exclude all records with an assertion that the scientific name provided does not ma… "-ass…            1
    ## 3    97 TRUE    "Exclude all records with an assertion that the scientific name provided is not stru… "-ass…            2
    ## 4    98 TRUE    "Exclude all records with an assertion that the name and classification supplied can… "-ass…            3
    ## 5    99 TRUE    "Exclude all records with an assertion that kingdom provided doesn't match a known k… "-ass…            4
    ## 6   100 TRUE    "Exclude all records with an assertion that the scientific name provided in the reco… "-ass…            5
