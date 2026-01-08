# Precompile vignettes to avoid risk of CRAN failures

# setup
devtools::load_all()
library(knitr)
library(dplyr)
galah_config(email = "ala4r@ala.org.au") # add your email

# workflow for automated detection and processing of vignettes
folder <- "./vignettes/"
all_files <- list.files(folder)
selected_files <- paste0(folder, all_files[grepl(".orig$", all_files)])
out_files <- sub(".orig$", "", selected_files)

purrr::map(
  seq_along(selected_files), 
  \(a){
         knit(selected_files[[a]], out_files[[a]])
})

## render README.md
# knitr::knit("README.Rmd", "README.md")

# # next steps
devtools::build_vignettes()
pkgdown::build_site()