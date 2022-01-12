# Precompile vignettes to avoid risk of CRAN failures

library(galah)
galah_config(email = "email@email.com") # add your email

library(knitr)
# knit("vignettes/galah.Rmd.orig", 
#      "vignettes/galah.Rmd")
# knit("vignettes/international_atlases.Rmd.orig",
#      "vignettes/international_atlases.Rmd")
# knit("vignettes/taxonomic_information.Rmd.orig",
#      "vignettes/taxonomic_information.Rmd")
# knit("vignettes/revamped_syntax.Rmd.orig",
#      "vignettes/revamped_syntax.Rmd")

# workflow for automated detection and processing of vignettes
folder <- "./vignettes/"
all_files <- list.files(folder)
selected_files <- paste0(folder, all_files[grepl(".orig$", all_files)])
out_files <- sub(".orig$", "", selected_files)

lapply(seq_along(selected_files), function(a){knit(
  selected_files[[a]],
  out_files[[a]]
)})

# # to render to HTML:
# devtools::build_vignettes()
# pkgdown::build_site()