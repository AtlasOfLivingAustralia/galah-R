# Precompile vignettes to avoid risk of CRAN failures

# setup
library(knitr)
library(dplyr)
# galah_config(email = "email@email.com") # add your email

# workflow for automated detection and processing of vignettes
folder <- "./vignettes/"
all_files <- list.files(folder)
selected_files <- paste0(folder, all_files[grepl(".orig$", all_files)])
out_files <- sub(".orig$", "", selected_files)

lapply(seq_along(selected_files), function(a){knit(
  selected_files[[a]],
  out_files[[a]]
)})

## render README.md
# knitr::knit("README.Rmd", "README.md")

# # next steps
devtools::build_vignettes()
pkgdown::build_site()
