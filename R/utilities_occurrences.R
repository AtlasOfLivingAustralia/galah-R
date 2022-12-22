get_doi <- function(mint_doi, data_path) {
  doi <- NA
  if (as.logical(mint_doi)) {
    tryCatch(
      doi <- as.character(
        read.table(unz(data_path, "doi.txt"))$V1),
      warning = function(e) {
        e$message <- "No DOI was generated for this download. The DOI server may
        be down or, if this is a cached result, may not have been generated for
        the original download."
      })
  }
  return(doi)
}

check_count <- function(count, max_count, error_call = caller_env()) {
  if (count < 1) {
    abort("This query does not match any records.", call = error_call)
  } else if (count > max_count) {
    too_many_records(max_count)
  } else {
    if (getOption("galah_config")$package$verbose) {
      count_text <- formatC(count, big.mark = ",", format = "f", digits = 0)
      inform(glue("This query will return {count_text} records"))
      }
  }
}

too_many_records <- function(max_count){
  max_text <- formatC(max_count, big.mark = ",", format = "f", digits = 0)
  bullets <- c(
    "Your data request was too large.",
    i = glue("A maximum of {max_text} records can be retrieved at once."),
    i = "Please narrow the query and try again."
  )
  abort(bullets, call = caller_env())
}


email_notify <- function() {
  notify <- as.logical(getOption("galah_config")$package$send_email)
  if (is.na(notify)) {
    notify <- FALSE
  }
  # ala api requires lowercase
  ifelse(notify, "true", "false")
}

user_email <- function(error_call = caller_env()) {
  email <- getOption("galah_config")$user$email
  if (email == "") {
    email <- Sys.getenv("email")
  }
  if (email == "") {
    bullets <- c(
      "No user email was found.",
      i = glue("To download occurrence records you must provide a valid email ",
                     "address registered with the selected atlas using `galah_config(email = )`")
    )
    abort(bullets, call = error_call)
  }
  email
}

occ_error_handler <- function(code, error_call = rlang::caller_env()) {
  if (code == 403) {
    bullets <- c(
      "Status code 403 was returned.",
      i = glue("Is the email you provided to `galah_config()` registered with the selected atlas?")
    )
    inform(bullets)
  }
  if (code == 504) {
    bullets <- c(
      "Status code 504 was returned.",
      i = "This usually means that the selected API is down.",
      i = "If you continue to receive this error, please email support@ala.org.au"
    )
    inform(bullets)
  }
}