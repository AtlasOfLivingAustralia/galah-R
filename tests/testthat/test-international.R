context("Test international atlas configuration")

test_that("International Atlases work as expected", {
  atlases <- supported_atlases()
  for (atlas in atlases) {
    expect_silent(ala_config(country = atlas))
    expect_gt(nrow(search_fields()), 1)
    expect_gt(ala_counts(), 0)
  }
})

# When adding a new Atlas it should be possible to use all ala_ functions
# Austria
# ala_species()
# ala_counts()
# ala_media()
# ala_occurrences()

# Guatemala
# ala_species() <- doesn't work
# ala_counts() <- tick
# ala_media() <- tick
# ala_occurrences() <- tick

# Spain
# ala_species() <- tick
# ala_counts() <- tick
# ala_media() <- tick
# ala_occurrences() <- tick

# Sweden
# ala_species() <- tick
# ala_counts() <- tick
# ala_media() <- doesn't work
# ala_occurrences() <- tick

# UK
# ala_species() <- tick
# ala_counts() <- tick
# ala_media() <- tick
# ala_occurrences() <- tick



# reset to Aus
ala_config(country = "Australia")

