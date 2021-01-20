#' \pkg{koala}
#'
#' This project enables the R community to access data and tools hosted by the
#' Atlas of Living Australia. The goal of the project is
#' to enable basic species and related information to be queried and output in
#' standard formats for R. koala is based around the extensive web services
#' provided by the Atlas; see the API link below.
#'
#' If you have any questions or suggestions, please email support@ala.org.au.
#'
#' @name koala
#' @docType package
#' @references \url{https://api.ala.org.au/}
#' @import assertthat sp sf httr
#' @importFrom crul HttpClient Paginator
#' @importFrom digest digest
#' @importFrom grDevices dev.off pdf rainbow
#' @importFrom graphics image legend points title
#' @importFrom jsonlite fromJSON
#' @importFrom stats aggregate na.omit
#' @importFrom stringr regex str_c str_detect str_extract str_locate
#' str_match str_match_all
#' @importFrom stringr str_replace str_replace_all str_split str_trim str_match
#' @importFrom utils data packageVersion read.csv write.csv read.table str
#' unzip URLencode download.file setTxtProgressBar txtProgressBar tail
#' @importFrom wellknown lint
NULL
