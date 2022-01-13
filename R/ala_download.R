# Download a file
# so far needs to handle zip files and csv
ala_download <- function(url, path, params = list(), ext = ".csv",
                         cache_file = NULL) {
  tryCatch({
    internal_download(
      url = url,
      path = path,
      params = params,
      ext = ext,
      cache_file = cache_file)
    }, 
    error = function(a){return(NULL)},
    warning = function(a){return(NULL)}
  )
}

internal_download <- function(url, path, params, ext, cache_file, 
                              error_call = caller_env()) {
  assert_that(is.character(url))
  cli <- HttpClient$new(
    url = url,
    headers = list(
      "User-Agent" = galah_version_string()
    )
  )

  # check cache file exists
  if (!is.null(cache_file) && !dir.exists(dirname(cache_file))) {
    directory <- dirname(cache_file)
    bullets <- c(
      "Cannot find directory.",
      i = "Please enter a valid directory and try again.",
      x = glue("{directory} does not exist.")
    )
    abort(bullets, call = error_call)
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
    # error message is specific to atlas_species because it is the only function that
    # gets to this point
    tryCatch(
      df <- read.csv(res$content, stringsAsFactors = FALSE),
      error = function(e) {
        e$message <- inform("No species matching the supplied filters were found.")
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
