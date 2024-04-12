# Precompile vignettes to avoid risk of CRAN failures

# setup
devtools::load_all()
library(knitr)
library(dplyr)
galah_config(email = "ala4r@ala.org.au") # add your email

# get data on atlases for the `choosing an atlas` vignette
atlases <- show_all(atlases)
counts <- map(atlases$region, 
              function(x){
                galah_config(atlas = x)
                atlas_counts()
              })
services <- show_all(apis) |>
  group_by(atlas) |>
  summarize(n_services = n())

atlases_csv <- atlases |>
  select(region, institution) |>
  bind_cols(bind_rows(counts)) |>
  left_join(services, by = c("region" = "atlas")) |>
  arrange(desc(count))

write.csv(atlases_csv, "./vignettes/atlas_stats.csv", row.names = FALSE)


# workflow for automated detection and processing of vignettes
folder <- "./vignettes/"
all_files <- list.files(folder)
selected_files <- paste0(folder, all_files[grepl(".orig$", all_files)])
out_files <- sub(".orig$", "", selected_files)

lapply(
  seq_along(selected_files), 
  function(a){
         knit(selected_files[[a]], out_files[[a]])
})

## render README.md
# knitr::knit("README.Rmd", "README.md")

# # next steps
devtools::build_vignettes()
pkgdown::build_site()
