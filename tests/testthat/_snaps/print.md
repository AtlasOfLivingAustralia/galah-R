# object of class `data-request` formats correctly

    Code
      request_data()
    Message
      Object of type data_request containing:
      * type occurrences

# populated `data_request()` prints correctly

    Code
      filter(identify(galah_call(), "crinia"), year == 2025)
    Message
      Object of type data_request containing:
      * type occurrences
      * identify search_term: crinia
      * filter Object of class `data_filter`: year == 2025

# object of class `metadata-request` formats correctly

    Code
      request_data()
    Message
      Object of type data_request containing:
      * type occurrences

# object of class `metadata-request` formats correctly with `filter()

    Code
      filter(request_metadata(), list == "dr650")
    Message
      Object of type metadata_request containing:
      * type lists
      * filter Object of class `metadata_filter` with type `list` (n = 0 entries)

# object of class `metadata-request` formats correctly with `identify()

    Code
      identify(request_metadata(), "Crinia")
    Message
      Object of type metadata_request containing:
      * type taxa
      * identify search_term: Crinia

# object of class `metadata-request` formats correctly with `identify() |> unnest()

    Code
      unnest(identify(request_metadata(), "Crinia"))
    Message
      Object of type metadata_request containing:
      * type taxa-unnest
      * identify search_term: Crinia

# object of class `query` formats correctly

    Code
      as_query(identify(request_metadata(), "Crinia"))
    Message
      Object of class query with type metadata/taxa-single
      * url: https://api.ala.org.au/namematching/api/search?q=Crinia
      * select: ~tidyselect::any_of(~c("search_term", "scientific_name",
      "scientific_name_authorship", "taxon_concept_id", "taxon_concept_lsid",
      "authority", "usage_key", "guid", "canonical_name", "status", "rank",
      "match_type", "confidence", "time_taken", "vernacular_name", "issues",
      "kingdom", "phylum", "class", "order", "family", "genus", "species"))

# object of class `computed_query` formats correctly

    Code
      x
    Message
      Object of class computed query with type metadata/taxa-single
      * url: https://api.ala.org.au/namematching/api/search?q=Crinia

# object of class `query_set` formats correctly

    Code
      coalesce(filter(galah_call(), basisOfRecord == "HUMAN_OBSERVATION"))
    Message
      Object of class query_set containing 4 queries:
      * metadata/fields url: https://api.ala.org.au/occurrences/index/fields
      * metadata/assertions url: https://api.ala.org.au/occurrences/assertions/codes
      * metadata/reasons url: https://api.ala.org.au/logger/service/logger/reasons
      * data/occurrences url:
      https://api.ala.org.au/occurrences/occurrences/offline/...

# `galah_config()` formats correctly

    Code
      galah_config()
    Message
      `galah` package configuration:
      
      Package
      v verbose
      v run_checks
      x send_email
      v authenticate
      i directory: /var/folders/47/2_32ylzx64qgpyqlh7zkd_5h0000gn/T//Rtmpu7Kgdm
      
      User
      username [Not Provided]
      email
      password [Not Provided]
      download_reason_id 4
      
      Atlas
      Atlas of Living Australia (ALA), Australia
      

