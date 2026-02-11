# Taxonomic filtering

Taxonomic complexity can confound the process of searching, filtering,
and downloading records using `galah`, but there are a few ways to
ensure records are not missed.

``` r
library(galah)
library(dplyr)
```

``` r
galah_config(email = "your_email_here", verbose = FALSE)
```

### search_taxa()

[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
enables users to look up taxonomic names before downloading data, which
allows for disambiguating homonyms and checking that the search term
matches the taxon name in the ALA .
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
returns the scientific name, authorship, rank, and full classification
for the taxon matched to the provided search term.

``` r
search_taxa("Petroica boodang") |> gt::gt()
```

| search_term      | scientific_name             | scientific_name_authorship | taxon_concept_id                                                          | rank    | match_type | kingdom  | phylum   | class | order         | family      | genus    | species          | vernacular_name | issues  |
|------------------|-----------------------------|----------------------------|---------------------------------------------------------------------------|---------|------------|----------|----------|-------|---------------|-------------|----------|------------------|-----------------|---------|
| Petroica boodang | Petroica (Petroica) boodang | (Lesson, 1838)             | https://biodiversity.org.au/afd/taxa/a3e5376b-f9e6-4bdf-adae-1e7add9f5c29 | species | exactMatch | Animalia | Chordata | Aves  | Passeriformes | Petroicidae | Petroica | Petroica boodang | Scarlet Robin   | noIssue |

``` r
# Muscicapa chrysoptera is a synonym for the Flame Robin, Petroica phoenicea
# Guniibuu is the Yuwaalaraay Indigenous name for the Red-Capped Robin, Petroica goodenovii
search_taxa("Muscicapa chrysoptera", "Guniibuu") |> gt::gt()
```

| search_term           | scientific_name               | scientific_name_authorship | taxon_concept_id                                                          | rank    | match_type | kingdom  | phylum   | class | order         | family      | genus    | species            | vernacular_name | issues  |
|-----------------------|-------------------------------|----------------------------|---------------------------------------------------------------------------|---------|------------|----------|----------|-------|---------------|-------------|----------|--------------------|-----------------|---------|
| Muscicapa chrysoptera | Petroica (Littlera) phoenicea | Gould, 1837                | https://biodiversity.org.au/afd/taxa/fe74e658-4848-437a-a23d-f1001a198552 | species | exactMatch | Animalia | Chordata | Aves  | Passeriformes | Petroicidae | Petroica | Petroica phoenicea | Flame Robin     | noIssue |

  
Where homonyms exist,
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
will prompt users to clarify the search term by providing one or more
taxonomic ranks in a `tibble`. This example differentiates among the
genus Morganella in three kingdoms:

``` r
search_taxa("Morganella") |> gt::gt()
```

    ## Warning: Search returned multiple taxa due to a homonym issue.
    ## ℹ Please provide another rank in your search to clarify taxa.
    ## ℹ Use a `tibble` to clarify taxa, see `?search_taxa`.
    ## ✖ Homonym issue with "Morganella".

| search_term | issues  |
|-------------|---------|
| Morganella  | homonym |

``` r
search_taxa(tibble(kingdom = "Fungi", genus = "Morganella")) |> gt::gt()
```

| search_term      | scientific_name | scientific_name_authorship | taxon_concept_id                                   | rank  | match_type | kingdom | phylum        | class          | order      | family      | genus      | issues  |
|------------------|-----------------|----------------------------|----------------------------------------------------|-------|------------|---------|---------------|----------------|------------|-------------|------------|---------|
| Fungi_Morganella | Morganella      | Zeller                     | https://id.biodiversity.org.au/node/fungi/60091999 | genus | exactMatch | Fungi   | Basidiomycota | Agaricomycetes | Agaricales | Agaricaceae | Morganella | noIssue |

  

### identify()

[`identify()`](https://rdrr.io/r/graphics/identify.html) is similar to
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md),
except that it can be used within a piped workflow to retrieve counts,
species, or records e.g.

``` r
galah_call() |>
  identify("Petroica boodang") |>
  count() |>
  collect()
```

    ## # A tibble: 1 × 1
    ##    count
    ##    <int>
    ## 1 132981

``` r
galah_call(type = "species") |>
  identify("Muscicapa chrysoptera", "Guniibuu") |>
  collect() |> 
  gt::gt()
```

| taxon_concept_id                                                          | species_name                   | scientific_name_authorship | taxon_rank | kingdom  | phylum   | class | order         | family      | genus    | vernacular_name  |
|---------------------------------------------------------------------------|--------------------------------|----------------------------|------------|----------|----------|-------|---------------|-------------|----------|------------------|
| https://biodiversity.org.au/afd/taxa/10dbd908-00f3-4ec2-9a9c-a2fd4782eaf1 | Petroica (Petroica) goodenovii | (Vigors & Horsfield, 1827) | species    | Animalia | Chordata | Aves  | Passeriformes | Petroicidae | Petroica | Red-capped Robin |
| https://biodiversity.org.au/afd/taxa/fe74e658-4848-437a-a23d-f1001a198552 | Petroica (Littlera) phoenicea  | Gould, 1837                | species    | Animalia | Chordata | Aves  | Passeriformes | Petroicidae | Petroica | Flame Robin      |

``` r
galah_call() |>
  identify(tibble(kingdom = "Fungi", genus = "Morganella")) |>
  collect() |>
  head() |> 
  gt::gt()
```

    ## Retrying in 1 seconds.

| recordID                             | scientificName          | taxonConceptID                                     | decimalLatitude | decimalLongitude | eventDate  | occurrenceStatus | dataResourceName                                |
|--------------------------------------|-------------------------|----------------------------------------------------|-----------------|------------------|------------|------------------|-------------------------------------------------|
| 001ec30d-3376-4f63-ba32-b48bc3dd137d | Morganella purpurascens | https://id.biodiversity.org.au/node/fungi/60092001 | -33.66218       | 150.2708         | 2021-04-10 | PRESENT          | NSW BioNet Atlas                                |
| 005ef5cf-aae1-411c-8476-8ac01dc80e9b | Morganella compacta     | NZOR-6-128055                                      | -36.82343       | 175.0731         | NA         | PRESENT          | New Zealand Fungal and Plant Disease Collection |
| 0084789b-e04d-4742-95b5-2e3761d9fd9c | Morganella compacta     | NZOR-6-128055                                      | -38.03175       | 176.4870         | 2019-05-14 | PRESENT          | New Zealand Fungal and Plant Disease Collection |
| 00efd1aa-ebf2-4afb-bd4d-b76af6ff0207 | Morganella compacta     | NZOR-6-128055                                      | -41.09759       | 172.9346         | 2014-05-01 | PRESENT          | New Zealand Fungal and Plant Disease Collection |
| 0688cd3a-2954-45f7-9775-f83331d86519 | Morganella compacta     | NZOR-6-128055                                      | -36.92053       | 174.4585         | 1942-07-12 | PRESENT          | New Zealand Fungal and Plant Disease Collection |
| 08010c5f-19ee-46e2-b6e0-973a5e79d135 | Morganella compacta     | NZOR-6-128055                                      | -42.24482       | 171.3296         | 1986-04-20 | PRESENT          | New Zealand Fungal and Plant Disease Collection |

### filter()

[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) subsets
records by searching for exact matches to an expression, and may also be
used for taxonomic filtering e.g.

``` r
galah_call() |>
  filter(species == "Petroica boodang") |>
  count() |>
  collect()
```

    ## # A tibble: 1 × 1
    ##    count
    ##    <int>
    ## 1 132981

Alternatively, we could use
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) after
first checking taxonomy with
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md),
in place of [`identify()`](https://rdrr.io/r/graphics/identify.html):

``` r
robins <- search_taxa("Muscicapa chrysoptera", "Guniibuu") 

galah_call() |>
  filter(taxonConceptID == robins$taxon_concept_id) |>
  count() |>
  collect()
```

    ## # A tibble: 1 × 1
    ##   count
    ##   <int>
    ## 1 88434

It is also possible to specify several species at once using
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html):

``` r
aus_petroica <- c("Petroica boodang", "Petroica goodenovii", 
                  "Petroica phoenicea", "Petroica rosea",
                  "Petroica rodinogaster", "Petroica multicolor")

galah_call() |>
  filter(species %in% aus_petroica) |>
  group_by(species, vernacularName) |>
  count() |> 
  collect() |>
  gt::gt()
```

| species               | vernacularName              | count  |
|-----------------------|-----------------------------|--------|
| Petroica boodang      | Scarlet Robin               | 129172 |
| Petroica boodang      | Eastern Scarlet Robin       | 3593   |
| Petroica boodang      | South-western Scarlet Robin | 166    |
| Petroica boodang      | Tasmanian Scarlet Robin     | 50     |
| Petroica goodenovii   | Red-capped Robin            | 119914 |
| Petroica phoenicea    | Flame Robin                 | 88434  |
| Petroica rosea        | Rose Robin                  | 60101  |
| Petroica rodinogaster | Pink Robin                  | 15640  |
| Petroica rodinogaster | Mainland Pink Robin         | 60     |
| Petroica rodinogaster | Tasmanian Pink Robin        | 21     |
| Petroica multicolor   | Pacific Robin               | 6705   |

This can be useful in searching for paraphyletic or polyphyletic groups,
which cannot be done using
[`identify()`](https://rdrr.io/r/graphics/identify.html). For example,
to get counts of non-chordates:

``` r
galah_call() |>
  filter(kingdom == "Animalia", phylum != "Chordata") |>
  group_by(phylum) |>
  count() |>
  collect() |>
  head() |> 
  gt::gt()
```

| phylum        | count   |
|---------------|---------|
| Arthropoda    | 9061922 |
| Mollusca      | 1498353 |
| Annelida      | 320916  |
| Cnidaria      | 283139  |
| Echinodermata | 206043  |
| Porifera      | 132106  |

### filter(), identify(), and taxonomic ranks

Deciding between using
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
[`identify()`](https://rdrr.io/r/graphics/identify.html) in a query
comes down to how a record has been classified, and whether or not you
have the correct *unique* name and classification of the taxa of
interest.

The ALA has fields for the primary taxonomic ranks (`kingdom`, `phylum`,
`class`, `order`, `family`, `genus`, `species`) and some secondary ranks
(e.g. `subfamily`, `subgenus`), all of which may be used with
[`galah_filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
and
[`galah_identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md).
Additionally, there is a field named `scientificName`, which refers to
the lowest taxonomic rank to which a record has been identified e.g.

``` r
galah_call() |>
  identify(tibble(genus = "Pitta")) |>
  group_by(scientificName, taxonRank) |>
  count() |>
  collect() |>
  filter(!is.na(scientificName)) |>
  gt::gt()
```

| scientificName                              | taxonRank  | count |
|---------------------------------------------|------------|-------|
| Pitta (Pitta) versicolor                    | species    | 30041 |
| Pitta (Pitta) iris                          | species    | 6550  |
| Pitta (Erythropitta)                        | subgenus   | 883   |
| Pitta (Pitta) versicolor versicolor         | subspecies | 311   |
| Pitta (Erythropitta) erythrogaster          | species    | 181   |
| Pitta (Pitta) iris iris                     | subspecies | 83    |
| Pitta                                       | genus      | 68    |
| Pitta (Pitta) versicolor intermedia         | subspecies | 42    |
| Pitta (Pitta) versicolor simillima          | subspecies | 38    |
| Pitta (Pitta) iris johnstoneiana            | subspecies | 27    |
| Pitta (Erythropitta) erythrogaster digglesi | subspecies | 6     |

If, for instance, you have the correct species or subspecies name, then
searching for matches against the `species` and `subspecies` fields,
respectively, will provide more precise results. This is because the
field `scientificName` may include subgenera. If you’ve used
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md) to
get the ALA-matched name of a taxon and only want records identified to
a particular level of classification, searching for matches against
`scientificName` is recommended.

Paraphyletic or polyphyletic groups may contain taxa identified to
different taxonomic levels. In this case, it is simpler to use
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
and [`identify()`](https://rdrr.io/r/graphics/identify.html) rather than
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html). In the
example below,
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
matches terms to one genus, three species, and two subspecies. This can
then be used in a piped workflow with
[`identify()`](https://rdrr.io/r/graphics/identify.html).

``` r
tas_endemic <- c("Sarcophilus", # Tasmanian Devil
                 "Bettongia gaimardi", # Tasmanian Bettong
                 "Melanodryas vittata", # Dusky Robin
                 "Platycercus caledonicus",# Green Rosella
                 "Aquila audax fleayi", # Tasmanian Wedge-Tailed Eagle
                 "Tyto novaehollandiae castanops") # Tasmanian Masked Owl

search_taxa(tas_endemic) |> gt::gt()
```

| search_term                    | scientific_name                       | scientific_name_authorship | taxon_concept_id                                                          | rank       | match_type | kingdom  | phylum   | class    | order           | family       | genus       | species                 | vernacular_name              | issues                                               |
|--------------------------------|---------------------------------------|----------------------------|---------------------------------------------------------------------------|------------|------------|----------|----------|----------|-----------------|--------------|-------------|-------------------------|------------------------------|------------------------------------------------------|
| Sarcophilus                    | Sarcophilus                           | Cuvier, 1837               | https://biodiversity.org.au/afd/taxa/06455b77-7d50-4ec7-9122-8ab48cfb0c1c | genus      | exactMatch | Animalia | Chordata | Mammalia | Dasyuromorphia  | Dasyuridae   | Sarcophilus | NA                      | NA                           | noIssue, noIssue, noIssue, noIssue, noIssue, noIssue |
| Bettongia gaimardi             | Bettongia gaimardi                    | (Desmarest, 1822)          | https://biodiversity.org.au/afd/taxa/8f7da937-6338-4c39-8b11-4f83807afe11 | species    | exactMatch | Animalia | Chordata | Mammalia | Diprotodontia   | Potoroidae   | Bettongia   | Bettongia gaimardi      | Tasmanian Bettong            | noIssue, noIssue, noIssue, noIssue, noIssue, noIssue |
| Melanodryas vittata            | Melanodryas (Amaurodryas) vittata     | (Quoy & Gaimard, 1830)     | https://biodiversity.org.au/afd/taxa/0f04889f-5489-4369-a545-8a041fba9f6d | species    | exactMatch | Animalia | Chordata | Aves     | Passeriformes   | Petroicidae  | Melanodryas | Melanodryas vittata     | Dusky Robin                  | noIssue, noIssue, noIssue, noIssue, noIssue, noIssue |
| Platycercus caledonicus        | Platycercus (Platycercus) caledonicus | (Gmelin, 1788)             | https://biodiversity.org.au/afd/taxa/c6e478fe-f199-463f-8576-a77108fd73e2 | species    | exactMatch | Animalia | Chordata | Aves     | Psittaciformes  | Psittacidae  | Platycercus | Platycercus caledonicus | Green Rosella                | noIssue, noIssue, noIssue, noIssue, noIssue, noIssue |
| Aquila audax fleayi            | Aquila (Uroaetus) audax fleayi        | Condon & Amadon, 1954      | https://biodiversity.org.au/afd/taxa/ac93f7f0-0686-4589-801a-5832378cb7c1 | subspecies | exactMatch | Animalia | Chordata | Aves     | Accipitriformes | Accipitridae | Aquila      | Aquila audax            | Tasmanian Wedge-tailed Eagle | noIssue, noIssue, noIssue, noIssue, noIssue, noIssue |
| Tyto novaehollandiae castanops | Tyto novaehollandiae castanops        | (Gould, 1837)              | https://biodiversity.org.au/afd/taxa/2c30d58b-572b-4dab-8644-b222c28eb0ec | subspecies | exactMatch | Animalia | Chordata | Aves     | Strigiformes    | Tytonidae    | Tyto        | Tyto novaehollandiae    | Tasmanian Masked Owl         | noIssue, noIssue, noIssue, noIssue, noIssue, noIssue |

``` r
galah_call() |>
  identify(tas_endemic) |>
  group_by(scientificName) |>
  count() |>
  collect() |>
  arrange(scientificName) |>
  gt::gt()
```

| scientificName                                    | count |
|---------------------------------------------------|-------|
| Aquila (Uroaetus) audax fleayi                    | 5035  |
| Bettongia gaimardi                                | 1964  |
| Bettongia gaimardi cuniculus                      | 41    |
| Bettongia gaimardi gaimardi                       | 9     |
| Melanodryas (Amaurodryas) vittata                 | 15719 |
| Melanodryas (Amaurodryas) vittata kingi           | 16    |
| Melanodryas (Amaurodryas) vittata vittata         | 46    |
| Platycercus (Platycercus) caledonicus             | 51180 |
| Platycercus (Platycercus) caledonicus brownii     | 24    |
| Platycercus (Platycercus) caledonicus caledonicus | 33    |
| Sarcophilus                                       | 3     |
| Sarcophilus harrisii                              | 36376 |
| Tyto novaehollandiae castanops                    | 67    |
