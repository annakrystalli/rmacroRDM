
#' Setup inputs folder.
#' 
#' Sets up input folder with correct folder structure
#' @param input.folder file path to input data folder.
#' @param meta.vars vector containing names of meta.vars. Defaults to c("qc", "observer", "ref", "n", 
#' "notes")
#' @keywords meta
#' @details Creates required folders in the data input folder for automated retrieval and processing 
#' of data. "raw" is a folder to stored raw data in. Clean data ready to compile should be stored in
#' the "csv" folder. Folder "r data" is used for a variety of data generated throughout the matching 
#' process. The metadata folder should contain a "metadata.csv" with information on all variables in 
#' the master datasheet. A folder for each observation metadata variable is also generated according
#' to the meta.vars vector supplied.
#' @export
#' @examples
#' setupInputFolder()

setupInputFolder <- function(input.folder, meta.vars = c("qc", "observer", "ref", "n", "notes")){
  
  if(!file.exists(input.folder)){stop("invalid input.folder path")}
  
  if(substr(input.folder, nchar(input.folder), nchar(input.folder)) != "/"){
    input.folder <- paste(input.folder, "/", sep = "")
  }
  
  lapply(c("raw", "csv", "metadata", "r data", "taxo", meta.vars),
         FUN = function(x){dir.create(paste(input.folder, x, sep =""), 
                                      showWarnings = F)})
}

#' Create metadata list
#' 
#' Creates a named metadata list of the appropriate length to match supplied vector of meta.vars
#' @param meta.vars vector containing names of meta.vars
#' @keywords meta 
#' @export
#' @return named list the same length as meta.vars.
#' @examples
#' createMeta()

createMeta <- function(meta.vars){
  
  meta <- vector("list", length(meta.vars))
  names(meta) <- meta.vars
  
  return(meta)
}

#############################################

#' Separate meta.vars
#' 
#' Takes m match object. Separates meta variables from m$data into a `meta` dataframe and assigns
#' it to appropriate slot in m$meta list. 
#' Columns are separated if their name matches a meta.var 
#' or if they are appended with the appropriate meta variable label (eg ..._ref). 
#' @param m match object
#' @return updated match object containing separated data and metadata data.frames.
#' @keywords meta 
#' @export
#' @examples
#' separateDatMeta()

separateDatMeta <- function(m){
  
  meta <- m$meta
  data <- m$data
  
  for(meta.var in names(meta)){
    
    if(any(names(data) == meta.var)){
      # if meta.var data for all variables is single column named `meta.var`
      meta[[meta.var]] <- data.frame(species = data$species, all = data[, names(data) == meta.var])
      data <- data[, !names(data) == meta.var]
    }else{# if meta.var data for individual variables is in columns named `_meta.var`
      if(length(names(data)[grep(paste("_", meta.var, sep = ""), names(data))]) > 0){
        vmeta.var <- names(data)[grep(paste("_", meta.var, sep = ""), names(data))]
        meta[[meta.var]] <- data[, c("species", vmeta.var)]
        names(meta[[meta.var]]) <- gsub(paste("_",meta.var, sep = ""), "", names(meta[[meta.var]]))
        data <- data[, !names(data) %in% vmeta.var]
      }
    }}
  
  m$data <- data
  m$meta <- meta
  
  return(m)
}


#############################################

#' Get metadata values associated with observations
#' 
#' Extracts appropriate metadata values for specified species vs variable data points. 
#' Data points specified by the combination of vectors of species and variable names.   
#' @param input.folder file path to input data folder.
#' @param meta.var name of meta.var
#' @param meta metadata list
#' @param spp vector of species names, must be same length as var
#' @param var vector of variable names, must be same length as spp
#' @keywords meta 
#' @return vector the same length as spp & var containg the meta.var data for the spp / var combinations specified
#' @export
#' @examples
#' getMeta()


getMeta <- function(meta.var, meta = meta, spp = spp, var = var){
  
  if(is.null(meta[[meta.var]])){return(NA)}
  if(is.null(dim(meta[[meta.var]]))){return(meta[[meta.var]])}else{
    if("all" %in% names(meta[[meta.var]])){
      return(meta[[meta.var]]$all[match(spp, meta[[meta.var]]$species)])}else{
        return(meta[[meta.var]][cbind(match(spp, meta[[meta.var]]$species), 
                                      match(var, names(meta[[meta.var]])))])
      }
  }
  
}        

#############################################

#' Substitute reference code with full reference
#' 
#' Sustitutes reference codes with full references across cells of a data.frame
#' @param ref.codes data.frame containing coded reference data. 
#' @param ref.table data.frame containing code to full reference look up table. Columns must 
#' be named `code` and `ref`.
#' @keywords meta
#' @return original ref.table with codes substituted for full references. 
#' @export
#' @examples
#' code2FullRef()

code2FullRef <- function(ref.codes, ref.table){
  
  cols <- which(names(ref.codes) != "species")
  
  for(i in dim(ref.table)[1]:1){
    for(j in cols){
      ref.codes[,j]  <-  gsub(ref.table$code[i], ref.table$ref[i], ref.codes[,j])
      
    }}

  return(ref.codes)
}

#############################################

#' Compile metadata
#' 
#' Compiles and checks available meta data. If none supplied, will check for a .csv file named after original 
#' data file and saved in the appropriate meta.var folder. Once compiled, checks whether meta variable are
#' appropriately allocated to data columns. Allows the assignment of metav columns to more 
#' than one data variable columns through a lookup .csv file named after original 
#' data file with the extension "_group.csv" and saved in the appropriate meta.var folder. If "_group.csv" not
#' present, will be created if no meta.var columns match data varnames. Returns Produces
#' appropriately named data.frame with meta variables assigned to appropriate data variables. 
#' @param m match object
#' @param input.folder path to master data input folder. Required if metadata are to be opened from .csv. Defaults to NULL
#' @param fileEnconding character string: if non-empty declares the encoding used on a file (not a connection) 
#' so the character data can be re-encoded. Defaults to NULL
#' @keywords meta
#' @return updated meta list 
#' @export
#' @examples
#' compileMeta()
#' 
compileMeta <- function(m, input.folder = NULL, fileEncoding = NULL){
  
  meta <- m$meta
  data <- m$data
  
  for(meta.var in names(meta)){
  
    metav.dd <- meta[[meta.var]]
    
    # IF NULL, CHECK FOR & LOAD AVAILABLE METADATA
    # 1. if meta.var in meta NULL, check whether there is a corresponding meta.var file and assign
    # to metav.dd
    
    if(is.null(metav.dd)){
      if(is.null(m$filename)){}else{
        if(!paste(m$filename, "csv", sep = ".") %in% list.files(paste(input.folder, meta.var, "/", sep =""))){}else{
          
          if(is.null(fileEncoding)){
          metav.dd <- read.csv(paste(input.folder, meta.var, "/", m$filename, ".csv",sep = ""), 
                               stringsAsFactors = F)}else{
                                 metav.dd <- read.csv(paste(input.folder, meta.var, "/", m$filename, ".csv",sep = ""), 
                                                      stringsAsFactors = F, fileEncoding = fileEncoding)}
          
          # clean loaded metav.dd
          while(sum(na.omit(metav.dd == " ")) > 0){metav.dd[metav.dd == " "] <- ""}
          metav.dd[metav.dd == ""] <- NA
        }}}

  # Process available metadata and add to meta
  if(!is.null(metav.dd)){
    
    # expand single value to `all` column
    if(length(metav.dd) == 1){meta[[meta.var]] <- data.frame(species = data$species, all = metav.dd)}else{
      
      # trim to single column for `all`
      if("all" %in% names(metav.dd)){meta[[meta.var]] <- metav.dd[,c("species", "all")]}else{
        
        # MATCHING METADATA COLUMNS TO DATA COLUMNS
        # vector of data vars to check for metadata
        check.vars <- names(data)[!(names(data) %in% "species")]
        
        # if there are metadata for all data variables, return check.vars metadata
        if(all(check.vars %in% names(metav.dd))){meta[[meta.var]] <- metav.dd[,c("species", check.vars)]}else{
          
          # if not all data vars have metadata matches, it is likely that metadata variables contain data from multiple 
          # data columns. Information should be provided in a meta.var group look up table. check whether it exists.
          if(length(grep(paste("_", meta.var, "_group", sep = ""), 
                         grep(gsub(".csv", "",m$filename), 
                              list.files(paste(input.folder, meta.var, "/", sep ="")), 
                              value = T)
                         )
                    ) == 0){
            
            # if group look up table does not exist, create csv in which individual data variables can be assigned 
            # to meta group names. If data variable has no metadata, leave as NA in group .
            metav.grp <- data.frame(var = check.vars, grp = "")
            metav.grp$metav.grp[metav.grp$var %in% names(metav.dd)] <- metav.grp$var[metav.grp$var %in% names(metav.dd)]
            write.csv(metav.grp, paste(paste(input.folder, meta.var, "/", sep =""), 
                                          gsub(".csv", "", m$filename), "_", meta.var, "_group.csv", 
                                          sep = ""),
                      row.names = F)
            stop(paste(meta.var,".group file created, ","update _",meta.var,
                       ".group file to proceed", sep = ""))}else{
                         
                         
                         # load csv in which meta group names are assigned to individual data variables
                         metav.grp  <- read.csv(paste(paste(input.folder, meta.var, "/", sep =""), 
                                                         gsub(".csv", "",m$filename), "_", 
                                                         meta.var, "_group.csv", sep = ""), 
                                                   stringsAsFactors = F)
                         # Check that all variables are assigned to valid meta data column or NA in the case 
                         # of no data. Stop if not.
                         if(!all(na.omit(metav.grp$grp) %in% names(metav.dd))){
                           stop(paste("assigned meta group names does not match supplied meta data names, update _",
                                      meta.var,".group file to proceed", sep = ""))}else{
                                        
                                        # Make sure ALL variables have reference data
                                        if(meta.var == "ref" & any(is.na(metav.grp$grp))){
                                          stop(paste("variables missing reference column. update ", 
                                                     meta.var,".group file to proceed", sep = ""))
                                        }
                                        
                                        # Isolate variables to be assigned meta data. Create new dataframe containing the 
                                        # appropriate meta column for each variable. 
                                        # Name with data variables and update appropriate meta slot
                                        check.vars <- metav.grp$var[which(!is.na(metav.grp$grp))]
                                        dd <- data.frame(species = data$species, matrix(NA, nrow = dim(data)[1], 
                                                                                                   ncol = length(check.vars)))
                                        names(dd) <- c("species", check.vars)
                                        dd[,check.vars] <- metav.dd[match(dd$species, metav.dd$species),
                                                                                metav.grp$grp[match(check.vars, 
                                                                                                       metav.grp$var)]]
                                        print(paste(meta.var, "vars matched successfully to _meta.var_group"))
                                        meta[[meta.var]] <- dd
                                        }}}
        
        
        
      }}}
    
    if(is.null(meta[[meta.var]])){
      if(meta.var == "ref"){stop("Processing stopped: no reference information")}else{
        print(paste("Warning: NULL data for meta.var:", meta.var))
      }
    }
  }
  
  m$data <- data
  m$meta <- meta
  return(m)
  }
  

#' Load and clean data.
#' 
#' If m$data still NULL, loads data using m$filename. Cleans and performs basic checks on data. 
#' Prepares species names and removes var.omit variables
#' @param m match object
#' @param input.folder path to master data input folder
#' @param var.omit vector of character strings containing names of data variables to be ignored
#' @keywords data
#' @return updated match object
#' @export
#' @examples
#' processDat()
processDat <- function(m, input.folder = input.folder, var.omit){
  
  data <- m$data
  
  if(is.null(data)){
    data <- read.csv(paste(input.folder, "csv/", m$filename, ".csv", sep = ""),  
                     stringsAsFactors=FALSE)}
  
  
  if(anyDuplicated(data$species) > 0){stop("duplicate species name in match dat")}
  
  if(any(data$species == "")){
    data <- data[-which(data$species == ""),]}
  
  #Make sure there are no empty cells and replace any with NA cells
  data[which(data== "", arr.ind = T)] <- NA
  require(stringr)
  
  data$species <- gsub(" ", "_", data$species)
  
  data <- data[, !names(data) %in% var.omit]
  
  m$data <- data
  
  return(m)}

#' Manually add metadata to meta list
#' 
#' Allows manual updating of meta list.
#' @param meta meta list
#' @param add named list. Names must match meta.vars to be updated. Data in elements of list added
#' to appropriate meta.var element of meta.
#' @keywords meta
#' @return updated meta list
#' @export
#' @examples
#' addMeta()
addMeta <- function(meta, add){
  
  for(meta.var in names(add)){
    meta[[meta.var]] <- add[[meta.var]]
  }
  return(meta)
}
  
  
#' Check that variable metadata are complete
#' 
#' Checks that the metadata file contains metadata for all variables in data. Variable names in 
#' data (excluding species) are checked against the variable names in the first column of the
#' metadata .csv saved in the metadata folder.
#' @param m match object
#' @param metadata variable metadata data.frame
#' @keywords meta
#' @return if metadata complete, returns m. if not, produces error.
#' @export
#' @examples
#' checkVarMeta()
checkVarMeta <- function(m, metadata){
  
  if(!all(names(m$data) %in% c(metadata$code, "species"))){
    print(names(m$data)[!(names(m$data) %in% c(metadata$code, "species"))])
    stop("metadata missing for some variables, metadata file needs updating")
  }else{
    print(paste(m$data.ID, "metadata complete"))
    return(m)
  }
}
  

# extracts taxonomic information for species. Matches to original taxonomy used on project so added  
# species are matched using parent.spp or syns information
spp2taxoMatch <- function(spp, parent.spp, taxo.table){
  
  if(is.null(taxo.table)){
    spp2taxo <- read.csv("r data/spp_to_taxo.csv", stringsAsFactors = F)}else{
      spp2taxo <- taxo.table
    }
  
  spp.id <- spp %in% spp2taxo$species
  pspp.id <- parent.spp %in% spp2taxo$species
  
  taxo.id <- spp.id == T | pspp.id == T
  
  if(!all(taxo.id)){
    stop(c("no spp2taxo data for species", unique(spp[!taxo.id])))}else{
      if(all(spp.id)){
        dat <- spp2taxo[match(spp, spp2taxo$species),]
      }else{
        sppp <- spp
        p <- parent.spp %in% spp2taxo$species & !spp %in% spp2taxo$species
        sppp[p] <- parent.spp[p]
        dat <- spp2taxo[match(sppp, spp2taxo$species),]
        
        if(is.null(taxo.table)){  
          # Update spp2taxo file
          add.dat <- cbind(species = spp[p],spp2taxo[match(parent.spp[p], spp2taxo$species),-1])
          add.dat$subspp <- TRUE
          add.dat$parent.spp <- parent.spp[p]
          write.csv(rbind(spp2taxo, add.dat), "r data/spp_to_taxo.csv", row.names = F)}
      }
    }
  
  dat <- data.frame(species = spp, dat[,c("order", "family")])  
  
  return(dat)
}

# Compiles dataset into format compatible with appending to master database. Takes 
# match object m.
masterDataFormat <-  function(m, input.folder, output.folder){
    
  data <- m$data
  
  #make vector of data variables to be added
  match.vars <- names(data)[!names(data) %in% c("species", "synonyms", "data.status")]
  match.dat <- data.frame(data[, match.vars])
  names(match.dat) <- match.vars
  
  #find non NA values in match data. Match arr.indices to spp and variable names (for QA)
  id <- which(!is.na(match.dat), arr.ind = T)
  spp <- as.character(data[,"species"][id[, "row"]])
  var <- as.character(match.vars[id[, "col"]])
  
  mdat <- try(cbind(species = spp,
                    var = var, 
                    value = match.dat[id], data = m$data.ID,
                    synonyms = data[id[,"row"], "synonyms"], 
                    data.status = data[id[,"row"], "data.status"],
                    qc = getMeta("qc", meta = m$meta, spp = spp, var = var),
                    observer = getMeta("observer", meta = m$meta, spp = spp, var = var),
                    ref = getMeta("ref", meta = m$meta, spp = spp, var = var),
                    n = getMeta("n", meta = m$meta, spp = spp, var = var)))
    
    return(list(mdat = mdat, spp.list = m$spp.list))
}


# look up unmatched species in table (lookup.dat) of known match pairs. 
# If list is of unmatched data species (ie dataset species is a subset of spp.list), 
# match to known synonyms. If list is of unmatched spp.list species (ie spp.list a subset of 
# data$species), match to known species. 
sppMatch <- function(m, unmatched = unmatched, syn.links, addSpp = T){
  
  data <- m$data
  spp.list <- m$spp.list
  sub <- m$sub
  set <- m$set
  
  match.dd <- NULL
  
  for(spp in unmatched){
    
    syns <- getAllSyns(syn.links, spp)
    
    synsInSet <- syns %in% get(set)$species
    
    if(length(syns[synsInSet]) == 0){match <- NULL}else{
      syns <- syns[synsInSet]
      synsNotInSub <- !syns %in% get(sub)$species
      if(sub == "spp.list"){
        if(length(syns[synsNotInSub]) > 0){
          match <- data.frame(species = spp, synonyms = syns[synsNotInSub][1])
        }else{
          match <- data.frame(species = spp, synonyms = syns[1])}}
      if(sub == "data"){
        if(length(syns[synsNotInSub] > 0)){
          match <- data.frame(species = syns[synsNotInSub][1], synonyms = spp)}else{
            match <- NULL
            if(addSpp){
              spp.list <- rbind(spp.list, data.frame(species = spp, master.spp = F, rel.spp = syns[1]))}
          }
        
      }
    }
    
    match.dd <- rbind(match.dd, match)
  }
  
  if(is.null(match.dd)){}else{
    add.dd <- cbind(match.dd, data[match(match.dd$synonyms, data$species), 
                                   !names(data) %in% c("species", "synonyms")])
    add.dd$data.status <- "modified"
    data <- rbind(data, add.dd)}
  
  m$data <- data
  m$spp.list <- spp.list
  
  return(m)}


# match data set to master species list using all available known match pair tables.
dataSppMatch <- function(m, ignore.unmatched = ignore.unmatched, syn.links = syn.links, 
                         addSpp = T, save.m = F){
  
  sub <- m$sub
  set <- m$set
  
  # Check whether matching required and match
  unmatched <- m[[sub]]$species[!(m[[sub]]$species %in% m[[set]]$species)]
  if(length(unmatched) == 0){print(paste(m$data.ID, "data direct match to spp.list, no further matching required"))
    m$status <- "full_match"
    return(m)}
  
  # remove extinct or new species from data to be added
  if(sub == "data"){
    rm <- c(synSets(syn.links, spp = "Extinct"), synSets(syn.links, spp = "New"))
    m$data <- m$data[!(m$data$species %in% rm),]
  }
  
  m <- sppMatch(m, unmatched = unmatched, syn.links = syn.links, 
                addSpp = addSpp)
  
  #generate next unmatched species list
  unmatched <- m[[sub]]$species[!(m[[sub]]$species %in% m[[set]]$species)]
  
  # Trim data
  m$data <- m$data[m$data$species %in% m$spp.list$species,]
  
  # if no more species unmatched break loop
  if(length(unmatched) == 0){
    print(paste(m$data.ID, "match complete"))
    m$status <- "full_match"
  }else{
    
    print(paste("match incomplete,",length(unmatched), sub, "datapoints unmatched"))
    m$status <- paste("incomplete_match:", length(unmatched)) 
    
    if(!ignore.unmatched){
    
      # if all match pair datasets checked and species remain unmatched write manual match spp list
      dir.create(paste(input.folder, "r data/", sep = ""), showWarnings = F)
      dir.create(paste(input.folder, "r data/match data/", sep = ""), showWarnings = F)
      
      write.csv(data.frame(synonyms = if(sub == "spp.list"){""}else{unmatched},
                           species = if(sub == "spp.list"){unmatched}else{""}),
                paste("r data/match data/",m$data.ID," mmatch.csv", sep = ""),
                row.names = F)
      print(paste("unmatched species list saved in file 'Data.IDmmatch.csv'"))
      stop("manually match and save as 'Data.IDmmatched.csv' to continue")}
  }
  
  if(save.m){
    dir.create(paste(output.folder, "data/", sep = ""), showWarnings = F)
    dir.create(paste(output.folder, "data/match objects/", sep = ""), showWarnings = F)
    
    save(m, file = paste(output.folder, "data/match objects/", m$data.ID, " match object.RData", 
                       sep = ""))}
  return(m)}





# Processes ITIS synonyms data into a species synonym dataset 
ITISlookUpData <- function(version=NULL){
  aves.names <- read.csv("r data/match data/Aves synonym data (ITIS).csv", stringsAsFactors=FALSE)
  aves.codes <- read.csv("r data/match data/spp code matches.csv", stringsAsFactors=FALSE)
  
  species <- aves.names$species[match(aves.codes$Main, aves.names$code)]
  synonyms <- aves.names$species[match(aves.codes$Synonym, aves.names$code)]
  
  itis.match <- data.frame(species, synonyms, stringsAsFactors = F)
  itis.match <- itis.match[complete.cases(itis.match),]
  
  if(version == 2){names(itis.match) <- c("synonyms", "species")}
  
  return(itis.match)}


#Look up unmantched vector of species. If vector contains unmatched data species, 
#Prepares data for matching of species to master species name. Creates synonyms column to link back to original data and 
# and data status, used to indicate data has bee prepared but also whether state of data row is original or has been added.

dataMatchPrep <- function(m){
  
  data <- m$data
  
  if("data.status" %in% names(data)){
    print("Data already prep-ed")
    return(data)
  }else{
    dt <- data.frame(data[,names(data) != "species"], stringsAsFactors = FALSE)
    names(dt) <- names(data)[names(data) != "species"]
    data <- data.frame(species = data$species, synonyms = data$species, 
                       data.status = "original", dt,
                       stringsAsFactors = FALSE)
    m$data <- data
    
    return(m)}
}


# Create match object
matchObj <- function(data.ID, spp.list, data, status = "unmatched", 
                     sub = data.match.params$sub[data.match.params$data.ID == data.ID],
                     meta = meta, filename = NULL, unmatched = NULL){
  
  
  if(sub == "spp.list"){
    set <- "data"
  }
  if(sub == "data"){
    set <- "spp.list"
  }   
  
  m <- list(data.ID = data.ID, spp.list = spp.list, data = data, sub = sub, 
            set = set, status = status, meta = meta, filename = filename)
  return(m)
}


synSets <- function(syn.links, spp){
  
  syns <- unique(unlist(
    syn.links[as.vector(
      unlist(apply(syn.links, 2,
                   FUN = function(x, spp){which(is.element(x, spp), arr.ind = T)},
                   spp))),]))
  
  syns <- syns[!syns %in% spp]
  return(syns)
}

getAllSyns <- function(syn.links, spp){
  
  syna <- synSets(syn.links, spp)
  syns <- syna
  
  while(length(syna) > 0){
    syna <- synSets(syn.links, syns)
    syna <- syna[!syna %in% c(spp, syns)]
    syns <- c(syns, syna)
  }
  
  return(syns)
}


addVars <- function(data, master){
  
  vars <- names(data)[!(names(data) %in% c("species", "synonyms", "data.status"))]
  
  for(var in vars){
    if(is.character(data[,var])){data[,var][data[,var]==""] <- NA}
    add <- data.frame(spp = match(data$species, master$species), 
                      dat = data[,var], stringsAsFactors = F)
    add <- add[complete.cases(add),]
    #if(!any(is.na(as.numeric(add$dat)))){add$dat <- as.numeric(add$dat)}
    
    var.col <- data.frame(rep(NA, dim(master)[1]))
    var.col[add[,"spp"],] <- add[,"dat"]
    
    names(var.col)<- var
    
    master <- data.frame(master, var.col, stringsAsFactors = F)}
  
  return(master)}

# Matches sppecies names to identifiers in data. ids needs to be a 2 column dataframe. 
# str = c(index, species), file = path to data to be matched with column spp_no instead of species.
# writes processed data file to csv, appending "_SPP" to file name.

IDSppMatch <- function(file = "Display & resource_scores.csv", 
                       ids = read.csv("r data/id_to_spp.csv", stringsAsFactors = F)){
  dd <- read.csv(paste("standardised csv data/", file, sep = ""), stringsAsFactors = F)
  dd <- dd[!apply(dd[,names(dd) != "spp_no"], 1, FUN = function(x){all(is.na(x))}),]
  
  ids <- ids
  dd <- data.frame(species = ids$species[match(dd$spp_no, ids$index)], dd[,names(dd) != "spp_no"])
  
  write.csv(dd, paste("standardised csv data/", gsub(".csv", "",file), "_SPP.csv",sep = ""),
            row.names = F)
  
}


# Tests whether a proposed synonym/species has a match in the spp.list/data and updates the mmatched file for the data set.
# Takes a match object (x) all information needed is stored within the file
testSynonym <- function(spp, x){
  #identify next species being matched and print
  mmatch <- read.csv(paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
                     stringsAsFactors=FALSE)  
  
  sub <- x$sub 
  
  
  if(sub == "spp.list"){
    set <- "data"
    lookup <- "species"
    lookupin <- "synonyms"
  }
  if(sub == "data"){
    set <- "spp.list"
    lookup <- "synonyms"
    lookupin <- "species"
  }   
  
  spp.m <- mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
  print(paste("match", lookup, spp.m))
  
  #test potential synonym 
  if(spp %in% c("Extinct","New")){
    print(paste("match", lookupin, spp))
    mmatch[mmatch[lookup] == spp.m, lookupin] <- spp
    next.spp <-mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
    
    print(paste("next", lookup, ":", next.spp))
    write.csv(mmatch, paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
              row.names = F)
  }else{
    spp <- gsub(" ","_", spp)
    match <- any(spp %in% x[[set]]$species)  
    print(paste("match", lookupin, spp))
    print(match)
    
    if(match){
      
      mmatch[mmatch[lookup] == spp.m, lookupin] <- spp
      next.spp <-mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
      
      print(paste("next", lookup, ":", next.spp))
      write.csv(mmatch, paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
                row.names = F)
      
    }}
  
}
whichNext <- function(x = output){
  #identify next species being matched and print
  mmatch <- read.csv(paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
                     stringsAsFactors=FALSE)  
  
  sub <- x$sub 
  
  
  if(sub == "spp.list"){
    set <- "data"
    lookup <- "species"
    lookupin <- "synonyms"
  }
  if(sub == "data"){
    set <- "spp.list"
    lookup <- "synonyms"
    lookupin <- "species"
  }   
  
  spp.m <- mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
  print(paste("match", lookup, spp.m))
  
}


