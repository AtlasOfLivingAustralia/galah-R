# Precompile vignettes to avoid risk of CRAN failures

library(galah)
galah_config(email = "email@email.com") # add your email

library(knitr)
knit("vignettes/galah.Rmd.orig", "vignettes/galah.Rmd")
knit("vignettes/international_atlases.Rmd.orig",
     "vignettes/international_atlases.Rmd")
knit("vignettes/taxonomic_information.Rmd.orig",
     "vignettes/taxonomic_information.Rmd")
knit("vignettes/revamped_syntax.Rmd.orig",
     "vignettes/revamped_syntax.Rmd")
