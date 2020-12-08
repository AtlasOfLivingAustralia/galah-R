#' \pkg{ALA4R}
#'
#' This project enables the R community to access data and tools hosted by the
#' Atlas of Living Australia. The goal of the project is
#' to enable basic species and related information to be queried and output in
#' standard formats for R. ALA4R is based around the extensive web services
#' provided by the Atlas; see the API link below.
#' 
#' Note: The next release of ALA4R will be version 2.0.0. This will be a major update to simplify the process of downloading data, make behaviour across functions consistent, and remove redundant functionality.  
#' In the coming weeks, you will be able to test out the new functionality in the dev branch.
#' If you have any questions or suggestions, please email support@ala.org.au.
#'
#' @name ALA4R
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
#' unzip URLencode download.file setTxtProgressBar txtProgressBar
#' @importFrom wellknown lint
NULL
