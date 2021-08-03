# Download a file
# so far needs to handle zip files and csv
ala_download <- function(url, path, params = list(), ext = ".csv",
                         cache_file = NULL) {
  assert_that(is.character(url))
  cli <- HttpClient$new(
    url = url,
    headers = list(
      "User-Agent" = galah_version_string()
    )
  )

  # check cache file exists
  if (!is.null(cache_file) && !dir.exists(dirname(cache_file))) {
    stop(dirname(cache_file),
         " does not exist. Please create it and try again.")
  }

  # ws needs to be added for 
  if (!is.na(url_parse(url)$path) & !grepl("ws", path)) {
    path <- paste0(url_parse(url)$path,"/", path)
  }
  
  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, path, params)
    res <- cli$get(disk = cache_file)
  } else {
    cli$url <- url
    res <- cli$get(path = path, query = params, disk = cache_file)
  }

  if (ext == ".csv") {
    # error message is specific to ala_species because it is the only function which
    # gets to this point
    tryCatch(
      df <- read.csv(res$content, stringsAsFactors = FALSE),
      error = function(e) {
        e$message <- "No species matching the supplied filters were found."
        close(file(cache_file))
        unlink(cache_file)
        stop(e)
      }
    )
    
    close(file(cache_file))
    unlink(cache_file)
  } else {
    # for zipped files just return the path
    return(cache_file)
  }
  return(df)
}
