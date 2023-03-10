#' @importFrom readr read_csv read_tsv cols
url_download <- function(url, 
                         params = list(),
                         ext = "zip",
                         cache_file,
                         error_call = caller_env(),
                         data_prefix = "data|records") {
  
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
    }else{
      cache_dir <- dirname(cache_file)
      if(!grepl(paste0(".", ext, "^"), cache_file)){
        cache_file <- paste(cache_file, ext, sep = ".")
      }
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
                read_tsv(import_files, col_types = cols()) |>
                  suppressWarnings()
              }else{
                available_files <- all_files[grepl(".csv$", all_files) &
                                             grepl(paste0("^", data_prefix), all_files)]
                import_files <- paste(cache_dir, available_files, sep = "/")  
                result <- lapply(import_files, 
                       function(a){read_csv(a, col_types = cols()) |>
                                   suppressWarnings()}) |> 
                bind_rows()

                # add doi when mint_doi = TRUE
                if(any(all_files == "doi.txt")){
                  doi_file <- paste(cache_dir, "doi.txt", sep = "/")
                  attr(result, "doi") <- 
                    read.table(doi_file) |> as.character()
                }
                return(result)
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