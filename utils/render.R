rm(list=ls())

require(knitr)


# ---- folders ----
script.folder <- "~/Documents/workflows/Brain_size_evolution/"
report.folder <- "~/Google Drive/Brain evolution/outputs/reports/"


# ---- extract-scripts-from-Rmd ----
purl(input = "utils/temp_vignette.Rmd",
     output = "utils/temp_vignette.R")


# render to rmacroRDM 
rmarkdown::render("utils/temp_vignette.Rmd", 
                  output_format = "html_notebook", 
                  output_options = list(toc =  T,
                                        toc_float = T,
                                        number_sections =  T,
                                        theme = "paper"),
                  output_file = "~/Documents/workflows/rmacroRDM/temp_vignette.nb.html")
