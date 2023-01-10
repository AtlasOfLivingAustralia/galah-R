#' @importFrom readr read_csv read_tsv cols
url_download <- function(url, 
                         params = list(),
                         ext = "zip",
                         cache_file,
                         error_call = caller_env()) {

  assert_that(is.character(url))
  cli <- HttpClient$new(
    url = url,
    headers = list("User-Agent" = galah_version_string()))
  
  # check cache file exists
  if(missing(cache_file)){
    cache_dir <- tempfile()
    dir.create(cache_dir)
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
              unzip(cache_file, exdir = cache_dir)
              all_files <- list.files(cache_dir)
              if(is_gbif()){
                import_files <- paste(cache_dir, 
                                      all_files[grepl(".csv$", all_files)],
                                      sep = "/")
                read_tsv(import_files, col_types = cols())
              }else{
                import_files <- paste(cache_dir, 
                                      all_files[grepl("^data", all_files) & 
                                                grepl(".csv$", all_files)],
                                      sep = "/")              
                lapply(import_files, 
                       function(a){read_csv(a, col_types = cols())}) |> 
                bind_rows()
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