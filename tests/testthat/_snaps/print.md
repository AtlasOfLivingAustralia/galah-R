# object of class `data-request` formats correctly

    Code
      request_data()
    Message
      Object of type data_request containing:
      * type occurrences
      * atlas Australia

# populated `data_request()` prints correctly

    Code
      filter(identify(galah_call(), "crinia"), year == 2025)
    Message
      Object of type data_request containing:
      * type occurrences
      * atlas Australia
      * identify search_term: crinia
      * filter Object of class `data_filter`: year == 2025

# object of class `metadata-request` formats correctly

    Code
      request_data()
    Message
      Object of type data_request containing:
      * type occurrences
      * atlas Australia

# object of class `metadata-request` formats correctly with `filter()

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

# object of class `query` formats correctly

    Code
      capture(identify(request_metadata(), "Crinia"))
    Message
      Object of class query with type metadata/taxa-single
      * atlas: Australia
      * url: https://api.ala.org.au/namematching/api/search?q=Crinia

# object of class `computed_query` formats correctly

    Code
      x
    Message
      Object of class computed query with type metadata/taxa-single
      * atlas: Australia
      * url: https://api.ala.org.au/namematching/api/search?q=Crinia

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

# `galah_config()` formats correctly

    Code
      galah_config()
    Message
      `galah` package configuration
      
      Atlas
      Atlas of Living Australia (ALA), Australia
      
      User
      x authentication
      username [Provided]
      email ala4r@ala.org.au
      password [Provided]
      download_reason_id 10
      
      Package
      x verbose
      v run_checks
      x send_email
      v caching
      i directory: something
      

