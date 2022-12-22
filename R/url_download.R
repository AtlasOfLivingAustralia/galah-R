#' @importFrom readr read_csv read_tsv cols
url_download <- function(url, 
                         params = list(),
                         ext = "zip",
                         cache_file,
                         error_call = caller_env()) {
  
  is_gbif <- getOption("galah_config")$atlas$region == "Global"

  assert_that(is.character(url))
  cli <- HttpClient$new(
    url = url,
    headers = list("User-Agent" = galah_version_string()))
  
  # check cache file exists
  if(missing(cache_file)){
    cache_dir <- getOption("galah_config")$package$cache_directory
    if(!dir.exists(dirname(cache_dir))){
      dir.create(cache_file)
    }
    cache_file <- paste0(cache_dir, "/temp_file.", ext)
  }else{
    if(!dir.exists(dirname(cache_file))){
      bullets <- c(
        "Cannot find directory.",
        i = "Please enter a valid directory and try again.",
        x = glue("{directory} does not exist.")
      )
      abort(bullets, call = error_call)
    }
  }
  
  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, params)
    res <- try(cli$get(disk = cache_file), silent = TRUE)
  } else {
    res <- try(cli$get(query = params, disk = cache_file), silent = TRUE)
  }
  
  if(inherits(res, "try-error")){
    return(NULL)
  }else{
    
    df <- switch(ext, 
           # defaults to zip
           "zip" = {
              if(is_gbif){
                read_tsv(cache_file, col_types = cols())
              }else{
                read_csv(
                  unzip(cache_file, files = "data.csv"),  # note: for large files, there may be data1.csv, data2.csv etc
                  col_types = cols())
              }
           },
           # error message is specific to atlas_species because it is the only function that
           # gets to this point
           "csv" =  {tryCatch(
             read_csv(res$content, col_types = cols()),
             error = function(e) {
               e$message <- inform("No species matching the supplied filters were found.")
               close(file(cache_file))
               unlink(cache_file)
               stop(e)
             }
           )}
          )
    
    close(file(cache_file))
    unlink(cache_file)
    return(df)
  }
}