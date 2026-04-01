#' Read downloaded data from a zip file
#' 
#' @description
#' Living atlases supply data downloads as zip files. This function reads these
#' data efficiently, i.e. without unzipping them first, using the `readr` 
#' package. Although this function is mostly called internally, it is exported
#' to allow easy re-importing of downloaded files, without the need to re-run
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

  # FIXME: should be possible to autodetect GBIF files somehow
  # Ideally we shouldn't need the user to specify this.

  # check file
  check_zip_filename(file)
 
  # find data files _within_ the zip file
  data_files <- zip_data_file_names(file)

  # if none, abort
  if(length(data_files) < 1){
    cli::cli_abort("No data files detected")
  }

  # import correctly depending on the delimiter
  delim <- check_delimiter(file, data_files[1])
  result <- switch(delim, 
                   "tsv" = read_zip_gbif(file, data_files),
                   "csv" = read_zip_la(file, data_files))

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

#' get the delimiter for this file type
#' @noRd
#' @keywords Internal
check_delimiter <- function(file, data_file){
  # get header row of first file *only*
  x <- unz(description = file, 
      filename = data_file) |>
      readr::read_lines(n_max = 1)
  n_tabs <- stringr::str_count(x, "\t")
  n_commas <- stringr::str_count(x, ",")
  if(n_tabs > n_commas){
    "tsv"
  }else{
    "csv"
  }
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
