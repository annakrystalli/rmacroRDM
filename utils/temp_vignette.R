## ----global-setup, echo = F----------------------------------------------
rm(list=ls())
options(stringsAsFactors = F)

## ----source-rmacroRDM, message=FALSE, warning=FALSE----------------------
require(RCurl)
eval(parse(text = getURL("https://raw.githubusercontent.com/annakrystalli/rmacroRDM/master/R/functions.R", ssl.verifypeer = FALSE)))
eval(parse(text = getURL("https://raw.githubusercontent.com/annakrystalli/rmacroRDM/master/R/wideData_function.R", ssl.verifypeer = FALSE)))


## ----set-dirs------------------------------------------------------------

setDirectories(script.folder = "~/Documents/workflows/Brain_size_evolution/", 
               data.folder = "~/Google Drive/Brain evolution/",
               envir = globalenv())


## ----setup-file.system---------------------------------------------------

setupFileSystem(script.folder = "~/Documents/workflows/Brain_size_evolution/", 
                data.folder = "~/Google Drive/Brain evolution/")

## ----master-configuration, eval=T----------------------------------------
init_db(spp.list_src = "D0")

## ----setup-input.folder--------------------------------------------------
setupInputFolder(input.folder)

## ----ensure-fcodes-------------------------------------------------------
fcodes <- ensure_fcodes(meta.vars)


## ----set-file.names------------------------------------------------------
file.names <- create_file.names(file.names = c("brainmain2.csv", 
                                  "Amniote_Database_Aug_2015.csv", "anagedatasetf.csv"))



## ----load-sys.ref--------------------------------------------------------
load_sys.ref(fileEncoding = "mac", view = F)

## ----load-syn.links------------------------------------------------------
syn.links <- read.csv(text=getURL("https://raw.githubusercontent.com/annakrystalli/rmacroRDM/master/data/input/taxo/syn.links.csv", 
                                  ssl.verifypeer = FALSE), header=T)

## ----process-csvs, warning=FALSE-----------------------------------------

process_file.system(file.names, fcodes)


## ----create-spp.list-----------------------------------------------------
spp.list <- createSpp.list(species = NULL, 
                           taxo.dat = NULL, 
                           spp.list_src = spp.list_src)


## ----create-master-------------------------------------------------------
master <- create_master(spp.list)

## ----create-m------------------------------------------------------------

filename <- file.names[file.names == "Amniote_Database_Aug_2015.csv"]

m <- matchObj(file.name = filename,
              spp.list = master$spp.list,
              sub = "spp.list") # use addMeta function to manually add metadata.


## ----process-m-----------------------------------------------------------
m <- m %>% 
  separateDatMeta() %>% 
  compileMeta(input.folder = input.folder) %>%
  checkVarMeta(master$metadata) %>%
  dataMatchPrep()

## ----data-spp-match------------------------------------------------------
m <- dataSppMatch(m, syn.links = syn.links, addSpp = T)

## ----output--------------------------------------------------------------
output <- masterDataFormat(m, meta.vars, match.vars, var.vars)

## ----merge-to-master-----------------------------------------------------
master <- updateMaster(master, output = output)

