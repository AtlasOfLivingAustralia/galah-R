#' Read downloaded data from a zip file
#' 
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' Living atlases supply data downloads as zip files. This function reads these
#' data efficiently, i.e. without unzipping them first, using the `readr` 
#' package. Although this function has been part of galah for some time, it was 
#' previously internal to [atlas_occurrences()]. It has been exported now to 
#' support easy re-importing of downloaded files, without the need to re-run
#' a query.
#' @param file (character) A file name. Must be a length-1 character ending in
#' `.zip`.
#' @examples \dontrun{
#' # set a working directory
#' galah_config(directory = "data-raw",
#'              email = "an-email-address@email.com")
#' 
#' # download some data
#' galah_call() |>
#'   identify("Heleioporus") |>
#'   filter(year == 2022) |>
#'   collect(file = "burrowing_frog_data.zip")
#'   
#' # load data from file
#' x <- read_zip("./data-raw/burrowing_frog_data.zip")
#' }
#' @export
read_zip <- function(file){
  # basic checks
  if(missing(file)){
    rlang::abort("`file` is missing, with no default")
  }
  if(!is.character(file) | length(file) > 1){
    rlang::abort("Argument `file` should be a length-1 character")
  }
  if(!file.exists(file)){
    rlang::abort("`.zip` file not found")
  }
  if(!grepl(".zip$", file)){
    rlang::abort("`file` should end in `.zip`")
  }
  # get names of files stored in .zip
  all_files <- utils::unzip(file, list = TRUE)$Name
  # zip files contain a lot of metadata that `galah` does not import
  # import only those files that meet our criteria for 'data'
  if(is_gbif()){
    available_files <- all_files[grepl(".csv$", all_files)]
    result <- unz(description = file,  # require lapply?
                  filename = available_files) |> 
      readr::read_tsv(col_types = readr::cols()) |>
      suppressWarnings()
    # Note: DOIs for GBIF are stored in `compute()` stage, not in the zip file
  }else{
    available_files <- all_files[grepl(".csv$", all_files) &
                                   grepl("^data|records", all_files)]
    result <- lapply(available_files, 
                     function(a, x){
                       # create connection to a specific file within zip
                       conn <- unz(description = x, 
                                   filename = a, 
                                   open = "rb")
                       out <- readr::read_csv(conn, 
                                              col_types = readr::cols()) |>
                         suppressWarnings()
                       close(conn)
                       return(out)
                     }, x = file) |>
      dplyr::bind_rows()
    # # add doi when mint_doi = TRUE
    if(any(all_files == "doi.txt")){
      conn <- unz(description = file, 
                  filename = "doi.txt", 
                  open = "rb")
      attr(result, "doi") <- readr::read_file(conn) |>
        sub("\\n$", "", x = _)
      close(conn)
    }
    # look for citation in README.html
    # BUT first check whether other atlases do this
    if(any(all_files == "README.html")){
      conn <- unz(description = file, 
                  filename = "README.html", 
                  open = "rb")
      readme <- xml2::read_html(conn) |>
        xml2::as_list() |>
        unlist()
      close(conn)
      cite_check <- grepl("cite", names(readme))
      if(any(cite_check)){
        attr(result, "citation") <- readme[cite_check] |>
          glue_collapse(sep = "")
      }
    }
  }
  # add formatted date
  attr(result, "modified_date") <- file.info(file)$mtime |> 
    format("%e %B %Y") |>
    trimws()
  # exit safely
  if(is.null(result)){
    rlang::abort("No data loaded")
  }else{
    result
  }
}
