# Look up taxon information

[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
allows users to look up taxonomic names, and ensure they are being
matched correctly, before downloading data from the specified
organisation.

By default, names are supplied as strings; but users can also specify
taxonomic levels in a search using a `data.frame` or `tibble`. This is
useful when the taxonomic *level* of the name in question needs to be
specified, in addition to it's identity. For example, a common method is
to use the `scientificName` column to list a Latinized binomial, but it
is also possible to list these separately under `genus` and
`specificEpithet` (respectively). A more common use-case is to
distinguish between homonyms by listing higher taxonomic units, by
supplying columns like `kingdom`, `phylum` or `class`.

[`search_identifiers()`](https://galah.ala.org.au/R/reference/search_all.md)
allows users to look up matching taxonomic names using their unique
`taxonConceptID`. In the ALA, all records are associated with an
identifier that uniquely identifies the taxon to which that record
belongs. Once those identifiers are known, this function allows you to
use them to look up further information on the taxon in question.
Effectively this is the inverse function to
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md),
which takes names and provides identifiers.

Note that when taxonomic look-up is required within a pipe, the
equivalent to
[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md) is
[`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
(or
[`galah_identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)).
The equivalent to
[`search_identifiers()`](https://galah.ala.org.au/R/reference/search_all.md)
is to use
[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
to filter by `taxonConceptId`.

## Details

[`search_taxa()`](https://galah.ala.org.au/R/reference/search_all.md)
returns the taxonomic match of a supplied text string, along with the
following information:

- `search_term`: The search term used by the user. When multiple search
  terms are provided in a tibble, these are displayed in this column,
  concatenated using `_`.

- `scientific_name`: The taxonomic name matched to the provided search
  term, to the lowest identified taxonomic rank.

- `taxon_concept_id`: The unique taxonomic identifier.

- `rank`: The taxonomic rank of the returned result.

- `match_type`: (ALA only) The method of name matching used by the name
  matching service. More information can be found on the [name matching
  github
  repository](https://github.com/AtlasOfLivingAustralia/ala-name-matching?tab=readme-ov-file#understanding-the-name-matching-algorithm).

- `issues`: Any errors returned by the name matching service (e.g.
  homonym, indeterminate species match). More information can be found
  on the [name matching github
  repository](https://github.com/AtlasOfLivingAustralia/ala-name-matching?tab=readme-ov-file#error-types).

- `taxonomic names` (e.g. `kingdom`, `phylum`, `class`, `order`,
  `family`, `genus`)

When querying using
[`request_metadata()`](https://galah.ala.org.au/R/reference/galah_call.md),
you have the option to pass
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md)
within the query. The easiest way to do this is `select(everything())`,
but for completeness, the following additional fields are available:

- `success`: Logical indicating success or failure of the search

- `scientific_name_authorship`: Author and year for the name in question

- `name_type`: Usually `"SCIENTIFIC"`

- `lft` and `rgt`: Numeric indices for taxonomic lookups

- `species_group` and `species_subgroup`: List-columns giving group
  names

- `_id` fields for `rank` and any taxonomic rank fields (`kingdom_id`,
  `phylum_id` etc.)

## See also

[`search_all()`](https://galah.ala.org.au/R/reference/search_all.md) for
how to get names if taxonomic identifiers are already known.
[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md),
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md),
[`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
and [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
for ways to restrict the information returned by
[`atlas_()`](https://galah.ala.org.au/R/reference/atlas_.md) functions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search using a single string. 
# Note that `search_taxa()` is not case sensitive
search_taxa("Reptilia")

# Search using multiple strings. 
# `search_taxa()` will return one row per taxon
search_taxa("reptilia", "mammalia")

# Search using more detailed strings with authorship information
search_taxa("Acanthocladium F.Muell")

# Specify taxonomic levels in a tibble using "specificEpithet"
search_taxa(tibble::tibble(
  class = "aves", 
  family = "pardalotidae", 
  genus = "pardalotus", 
  specificEpithet = "punctatus"))

# Specify taxonomic levels in a tibble using "scientificName"                    
search_taxa(tibble::tibble(
  family = c("pardalotidae", "maluridae"), 
  scientificName = c("Pardalotus striatus striatus", "malurus cyaneus")))

# Use OOP for the same effect
# `identify()` tells the code that we want to search for _taxonomic_ metadata.
request_metadata() |>
  identify("crinia") |>
  collect()

# This approach has the advantage that we can call `select()`
request_metadata() |>
  identify("crinia") |>
  select(everything()) |>
  collect()

# Look up a unique taxon identifier
search_identifiers(query = "https://id.biodiversity.org.au/node/apni/2914510")

# OOP process for identifiers uses `filter()`, not `identify()`
# In these cases the `field` argument is used to specify `type`
request_metadata() |>
  filter(identifier = "https://id.biodiversity.org.au/node/apni/2914510") |>
  select(everything()) |>
  collect()
} # }
```
