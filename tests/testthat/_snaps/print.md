# Empty `data-request` formats correctly

    Code
      structure(list(), class = c("data_request", "list"))
    Message
      An empty object of type data_request

# object of class `data-request` formats correctly

    Code
      request_data()
    Message
      Object of type data_request containing:
      * type occurrences
      * atlas Australia

# populated `data_request()` prints correctly

    Code
      select(count(group_by(filter(identify(galah_call(), c("crinia", "uperoleia")),
      year == 2025), species)), species, count)
    Message
      Object of type data_request containing:
      * type occurrences
      * atlas Australia
      * identify search_terms: crinia + 1 more
      * filter Object of class `data_filter`: year == 2025
      * group_by species
      * count TRUE
      * select ~species | ~count

# object of class `metadata-request` formats correctly

    Code
      request_metadata()
    Message
      Object of type metadata_request containing:
      * type fields
      * atlas Australia

# populated `metadata_request()` prints correctly

    Code
      filter(request_metadata(), list == "dr650")
    Message
      Object of type metadata_request containing:
      * type lists
      * atlas Australia
      * filter Object of class `metadata_filter` with type `list`

# object of class `metadata-request` formats correctly with `identify()

    Code
      identify(request_metadata(), "Crinia")
    Message
      Object of type metadata_request containing:
      * type taxa
      * atlas Australia
      * identify search_term: Crinia

# object of class `metadata-request` formats correctly with `identify() |> unnest()

    Code
      unnest(identify(request_metadata(), "Crinia"))
    Message
      Object of type metadata_request containing:
      * type taxa-unnest
      * atlas Australia
      * identify search_term: Crinia

# object of class `files-request` formats correctly

    Code
      request_files()
    Message
      Object of type files_request containing:
      * type media
      * atlas Australia

# populated `files_request()` prints correctly

    Code
      filter(request_files(), media == tibble::tibble(media_id = 1234))
    Message
      Object of type files_request containing:
      * type media
      * atlas Australia
      * filter Object of class `files_filter` with 1 rows

# object of class `query` formats correctly

    Code
      capture(identify(request_metadata(), "Crinia"))
    Message
      Object of class query with type metadata/taxa-single
      * atlas: Australia
      * url: https://api.ala.org.au/namematching/api/search?q=Crinia
      * select: ~tidyselect::any_of(~c("search_term", "scientific_name",
      "scientific_name_authorship", "taxon_concept_id", "taxon_concept_lsid",
      "authority", "key", "usage_key", "guid", "canonical_name", "status", "rank",
      "match_type", "confidence", "time_taken", "vernacular_name", "issues",
      "kingdom", "phylum", "class", "order", "family", "genus", "species"))

# Object of class `query` containing `slice_head()` formats correctly

    Code
      

# object of class `query_set` formats correctly

    Code
      compound(filter(authenticate(galah_call(), email = "ala4r@ala.org.au"),
      basisOfRecord == "HUMAN_OBSERVATION"))
    Message
      Object of class query_set containing 5 queries:
      * metadata/config url: https://api.ala.org.au/common/api/getAuthConfig
      * metadata/fields url: https://api.ala.org.au/occurrences/index/fields
      * metadata/assertions url: https://api.ala.org.au/occurrences/assertions/codes
      * metadata/reasons url: https://api.ala.org.au/logger/service/logger/reasons
      * data/occurrences url:
      https://api.ala.org.au/occurrences/occurrences/offline/...

# object of class `computed_query` formats correctly

    Code
      x
    Message
      Object of class computed query with type metadata/taxa-single
      * atlas: Australia
      * url: https://api.ala.org.au/namematching/api/search?q=Crinia

# `galah_config()` formats correctly

    Code
      galah_config()
    Message
      `galah` package configuration
      
      Atlas
      Atlas of Living Australia (ALA), Australia
      
      User
      x authentication
      username [Not Provided]
      email random_email_for_testing@email.com
      password [Not Provided]
      download_reason_id 10
      
      Package
      x verbose
      v run_checks
      x send_email
      v caching
      i directory: something
      

