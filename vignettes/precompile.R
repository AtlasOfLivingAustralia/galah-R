# Precompile vignettes to avoid risk of CRAN failures

library(knitr)
knit("vignettes/galah.Rmd.orig", "vignettes/galah.Rmd")
knit("vignettes/international_atlases.Rmd.orig",
     "vignettes/international_atlases.Rmd")