#' Read downloaded data from a zip file
#' 
#' @description
#' Living atlases supply data downloads as zip files. This function reads these
#' data efficiently, i.e. without unzipping them first, using the `readr` 
#' package. Although this function has been part of galah for some time, it was 
#' previously internal to [atlas_occurrences()]. It has been exported now to 
#' support easy re-importing of downloaded files, without the need to re-run
#' a query.
#' @param file (character) A file name. Must be a length-1 character ending in
#' `.zip`.
#' @param source (character) Where was this file sourced from? Should be one 
#' of `"LA"` (for 'Living Atlas'; the default) or `"GBIF"`.
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
read_zip <- function(file, source = c("LA", "GBIF")){

  # FIXME: should be possible to autodetect GBIF files somehow
  # Ideally we shouldn't need the user to specify this.

  # check type, file
  source <- match.arg(source)
  check_zip_filename(file)
 
  # import file without unzipping
  data_files <- zip_data_file_names(file)
  result <- switch(source, 
                   "GBIF" = read_zip_gbif(file, data_files),
                   "LA" = read_zip_la(file, data_files))

  # add formatted date
  attr(result, "modified_date") <- file.info(file)$mtime |> 
    format("%e %B %Y") |>
    trimws()

  # exit safely
  if(is.null(result)){
    cli::cli_abort("No data loaded")
  }else{
    result
  }
}

#' Internal function to check the `file` argument to `read_zip()`
#' @noRd
#' @keywords Internal
check_zip_filename <- function(file){
   # basic checks
  if(missing(file)){
    cli::cli_abort("`file` is missing, with no default")
  }
  if(!is.character(file) | length(file) > 1){
    cli::cli_abort("Argument `file` should be a length-1 character")
  }
  if(!file.exists(file)){
    cli::cli_abort("`.zip` file not found")
  }
  if(!grepl(".zip$", file)){
    cli::cli_abort("`file` should end in `.zip`")
  }
}

#' Internal function to get file names from inside a zip file
#' @noRd
#' @keywords Internal
zip_data_file_names <- function(file){
  all_files <- utils::unzip(file, list = TRUE)$Name
  valid_check <- stringr::str_detect(all_files, ".csv$") &
                 !(all_files %in% c("citation.csv", "headings.csv"))
  all_files[valid_check]
}

#' Internal function to read a zip file from GBIF
#' @noRd
#' @keywords Internal
read_zip_gbif <- function(file, data_files){
  unz(description = file,  # require lapply?
      filename = data_files) |> 
    readr::read_tsv(col_types = readr::cols()) |>
    suppressWarnings()
}

#' Internal function to read a zip file from living atlases
#' @noRd
#' @keywords Internal
read_zip_la <- function(file, data_files){
  # read data
  result <- purrr::map(data_files, 
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
  if(any(data_files == "doi.txt")){
    conn <- unz(description = file, 
                filename = "doi.txt", 
                open = "rb")
    attr(result, "doi") <- readr::read_file(conn) |>
      sub("\\n$", "", x = _)
    close(conn)
  }

  # look for citation in README.html
  if(any(data_files == "README.html")){
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
        glue::glue_collapse(sep = "")
    }
  }
  result
}
