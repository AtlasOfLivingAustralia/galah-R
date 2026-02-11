# Biodiversity Data from the GBIF Node Network

The Global Biodiversity Information Facility (GBIF;
<https://www.gbif.org>) provides tools to enable users to find, access,
combine and visualise biodiversity data. `galah` is a `dplyr` extension
package that enables the R community to directly access data and
resources hosted by GBIF and several of it's subsidiary organisations
(known as 'nodes') using `dplyr` verbs.

The basic unit of data stored by these infrastructures is an
**occurrence** record, which is an observation of a biological entity at
a specific time and place. However, `galah` also facilitates access to
taxonomic information, or associated media such images or sounds, all
while restricting their queries to particular taxa or locations. Users
can specify which columns are returned by a query, or restrict their
results to observations that meet particular quality-control criteria.

For those outside Australia, 'galah' is the common name of *Eolophus
roseicapilla*, a widely-distributed Australian bird species.

## Functions

**Getting Started**

- [`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.md)
  Set package configuration options

- [`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md)/[`request_()`](https://galah.ala.org.au/R/reference/galah_call.md)
  Start to build a request

**Update a request object**

- [`apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md)
  Restrict to data that pass predefined checks

- [`arrange()`](https://galah.ala.org.au/R/reference/arrange.data_request.md)
  Arrange rows of a query on the server side

- [`authenticate()`](https://galah.ala.org.au/R/reference/authenticate.md)
  Authenticate your request via OAUTH in the browser

- [`count()`](https://galah.ala.org.au/R/reference/count.data_request.md)
  Request counts of the specified data type

- [`distinct()`](https://galah.ala.org.au/R/reference/distinct.data_request.md)
  Keep distinct/unique rows

- [`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md)
  Filter records (see also
  [`filter_object_classes`](https://galah.ala.org.au/R/reference/filter_object_classes.md)))

- [`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md)
  Spatial filtering of a query

- [`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) Get a
  glimpse of your data

- [`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
  Group counts by one or more fields

- [`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
  Search for taxonomic identifiers (see also
  [`taxonomic_searches`](https://galah.ala.org.au/R/reference/taxonomic_searches.md))

- [`select()`](https://galah.ala.org.au/R/reference/select.data_request.md)
  Fields to report information for

- [`slice_head()`](https://galah.ala.org.au/R/reference/slice_head.data_request.md)
  Choose the first n rows of a download

- [`unnest()`](https://galah.ala.org.au/R/reference/unnest.md) Expand
  metadata for `fields`, `lists`, `profiles` or `taxa`

**Create and execute a query**

- [`capture()`](https://galah.ala.org.au/R/reference/capture.data_request.md)
  Convert a request into a `prequery` or `query`

- [`compound()`](https://galah.ala.org.au/R/reference/compound.md)
  Convert an object into a `query_set` showing all calls needed for
  evaluation

- [`collapse()`](https://galah.ala.org.au/R/reference/collapse.data_request.md)
  Convert an object to a valid `query`

- [`compute()`](https://galah.ala.org.au/R/reference/compute.data_request.md)
  Compute a query

- [`collect()`](https://galah.ala.org.au/R/reference/collect.data_request.md)
  Retrieve a database query

**Wrappers for accessing data**

- [`show_all()`](https://galah.ala.org.au/R/reference/show_all.md) &
  [`search_all()`](https://galah.ala.org.au/R/reference/search_all.md)
  Data for generating filter queries

- [`show_values()`](https://galah.ala.org.au/R/reference/show_values.md)
  &
  [`search_values()`](https://galah.ala.org.au/R/reference/show_values.md)
  Show or search for values *within* `fields`, `profiles`, `lists`,
  `collections`, `datasets` or `providers`

- [`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.md)
  Download occurrence data

- [`atlas_counts()`](https://galah.ala.org.au/R/reference/atlas_.md) Get
  a summary of the number of records or species

- [`atlas_species()`](https://galah.ala.org.au/R/reference/atlas_.md)
  Download occurrences grouped by `speciesID`

- [`atlas_taxonomy()`](https://galah.ala.org.au/R/reference/atlas_.md)
  Download taxonomic trees

- [`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.md)
  Download media metadata linked to occurrences

- [`collect_media()`](https://galah.ala.org.au/R/reference/collect_media.md)
  Download media (images and sounds)

**Miscellaneous functions**

- [`atlas_citation()`](https://galah.ala.org.au/R/reference/atlas_citation.md)
  Get a citation for a dataset

- [`read_zip()`](https://galah.ala.org.au/R/reference/read_zip.md) To
  read data from an earlier download

- [`print()`](https://galah.ala.org.au/R/reference/print_galah_objects.md)
  Print functions for galah objects

## Terminology

To get the most value from `galah`, it is helpful to understand some
terminology. Each occurrence record contains taxonomic information, and
usually some information about the observation itself, such as its
location. In addition to this record-specific information, the living
atlases append contextual information to each record, particularly data
from spatial **layers** reflecting climate gradients or political
boundaries. They also run a number of quality checks against each
record, resulting in **assertions** attached to the record. Each piece
of information associated with a given occurrence record is stored in a
**field**, which corresponds to a **column** when imported to an
`tibble`. See `show_all(fields)` to view valid fields, layers and
assertions, or conduct a search using `search_all(fields)`.

Data fields are important because they provide a means to **filter**
occurrence records; i.e. to return only the information that you need,
and no more. Consequently, much of the architecture of `galah` has been
designed to make filtering as simple as possible. The easiest way to do
this is to start a pipe with
[`galah_call()`](https://galah.ala.org.au/R/reference/galah_call.md) and
follow it with the relevant `dplyr` function; starting with
[`filter()`](https://galah.ala.org.au/R/reference/filter.data_request.md),
but also including
[`select()`](https://galah.ala.org.au/R/reference/select.data_request.md),
[`group_by()`](https://galah.ala.org.au/R/reference/group_by.data_request.md)
or others. Functions without a relevant `dplyr` synonym include
[`identify()`](https://galah.ala.org.au/R/reference/identify.data_request.md)
for choosing a taxon, or
[`geolocate()`](https://galah.ala.org.au/R/reference/geolocate.md) for
choosing a specific location. By combining different filters, it is
possible to build complex queries to return only the most valuable
information for a given problem.

A notable extension of the filtering approach is to remove records with
low 'quality'. All living atlases perform quality control checks on all
records that they store. These checks are used to generate new fields,
that can then be used to filter out records that are unsuitable for
particular applications. However, there are many possible data quality
checks, and it is not always clear which are most appropriate in a given
instance. Therefore, `galah` supports data quality **profiles**, which
can be passed to
[`apply_profile()`](https://galah.ala.org.au/R/reference/apply_profile.md)
to quickly remove undesirable records. A full list of data quality
profiles is returned by `show_all(profiles)`.

## See also

Useful links:

- <https://galah.ala.org.au/R/>

- Report bugs at
  <https://github.com/AtlasOfLivingAustralia/galah-R/issues>

## Author

**Maintainer**: Martin Westgate <martin.westgate@csiro.au>

Authors:

- Dax Kellie <dax.kellie@csiro.au>

Other contributors:

- Shandiya Balasubramaniam <shandiya.balasubramaniam@csiro.au>
  \[contributor\]

- Matilda Stevenson \[contributor\]
