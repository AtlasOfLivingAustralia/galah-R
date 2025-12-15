# tests of dplyr syntax for converting occurrences to useful species-level 
# summaries within a single pipe.

# load libraries
library(galah)
library(dplyr)

# get example occurrence dataset
galah_config(email = "martinjwestgate@gmail.com")
testdata <- galah_call() |>
  identify("perameles") |>
  select(speciesID, group = "basic") |>
  filter(year == 2020) |>
  collect()

# number of records
testdata |> 
  count()

# number of records per species
testdata |>
  count(speciesID)

# number of species (aka `atlas_counts(type = "species")`)
testdata |>
  summarize(count = n_distinct(speciesID))

# one row per species (aka `atlas_species()`)
testdata |>
  distinct(speciesID, .keep_all = TRUE)
# Noting that this requires care with `select()` to return sensible results

# show only which species are present in the query (aka `show_values()`)
testdata |>
  distinct(speciesID, .keep_all = FALSE)

# get species (and optionally, record) counts grouped by a second factor
testdata |>
  group_by(basisOfRecord) |>
  summarize(n_records = n(),
            n_spp = n_distinct(speciesID))

# get one row per species, showing number of records
testdata |>
testdata |>
  group_by(speciesID) |>
  mutate(count = n()) |>
  distinct(.keep_all = TRUE)


# example species download for comparison purposes
# noting syntax may change
testspecies <- galah_call() |>
  identify("perameles") |>
  filter(year == 2020) |>
  group_by(speciesID) |>
  collect()