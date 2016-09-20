rm(list=ls())

require(knitr)


# ---- folders ----
script.folder <- "~/Documents/workflows/Brain_size_evolution/"
report.folder <- "~/Google Drive/Brain evolution/outputs/reports/"


# ---- extract-scripts-from-Rmd ----
purl(input = paste(script.folder, "match_workflow.Rmd", sep = ""),
     output = paste(script.folder, "match_workflow.R", sep = ""))

# ---- render-Rmd ----
rmarkdown::render(paste(script.folder, "match_workflow.Rmd", sep = ""), 
                  output_format = "html_notebook", 
                  output_options = list(toc =  T,
                                        toc_float = T,
                                        number_sections =  T,
                                        theme = "paper"))


# render to rmacroRDM 
rmarkdown::render(paste(script.folder, "match_workflow.Rmd", sep = ""), 
                  output_format = "html_notebook", 
                  output_options = list(toc =  T,
                                        toc_float = T,
                                        number_sections =  T,
                                        theme = "paper"),
                  output_file = "~/Documents/workflows/rmacroRDM/temp_vignette.nb.html")
