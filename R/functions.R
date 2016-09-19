pkgs <- c("dplyr", "taxize")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pkgs, character.only = T)


#' Setup file.system.
#' 
#' Sets up file.system with correct folder structure
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
#' setupFileSystem()

setupFileSystem <- function(script.folder = getwd(), data.folder,
                             migrate = F, ...){
  
  dir.create(paste(data.folder, "inputs", sep = ""), showWarnings = F)
  dir.create(paste(data.folder, "outputs", sep = ""), showWarnings = F)
  dir.create(paste(data.folder, "info", sep = ""), showWarnings = F)
  dir.create(paste(data.folder, "inputs/data", sep = ""), showWarnings = F)
  
  dir.create(paste(script.folder, "process", sep = ""), showWarnings = F)
  
  dir.create(paste(output.folder, "data", sep = ""), showWarnings = F)
  dir.create(paste(output.folder, "reports", sep = ""), showWarnings = F)
  dir.create(paste(output.folder, "figures", sep = ""), showWarnings = F)
  
}


setDirectories <- function(script.folder, data.folder, envir = globalenv()) {
  
  assign("script.folder", script.folder, 
         envir = envir)
  assign("input.folder", paste(data.folder, "inputs/data/", sep = ""), 
         envir = envir)
  assign("output.folder", paste(data.folder, "outputs/", sep = ""), 
         envir = envir)
}

#' Setup input.folder
#'
#'#' Sets up input.folder with correct folder structure
#' @param input.folder file path to input data folder.
#' 
#' @keywords meta
#' 
#' @details Creates required folders in the data input folder for automated retrieval and processing 
#' of data. "raw" is a folder to stored raw data in. Clean data ready to compile should be stored in
#' the "csv" folder. Folder "r data" is used for a variety of data generated throughout the matching 
#' process. The metadata folder should contain a "metadata.csv" with information on all variables in 
#' the master datasheet. A folder for each observation metadata variable is also generated according
#' to the meta.vars vector supplied. Requires that meta.vars have been configured. vector containing names of meta.vars. Defaults to c("qc", "observer", "ref", "n", 
#' "notes")

setupInputFolder <- function(input.folder, migrate = F){
  
  if(!file.exists(input.folder)){stop("invalid input.folder path")}
  if(!exists("meta.vars")){
    stop("meta.vars not configured. \n ensure data.base is initialised: see init_db()")
  }
  
  if(substr(input.folder, nchar(input.folder), nchar(input.folder)) != "/"){
    input.folder <- paste(input.folder, "/", sep = "")
  }
  
  # create data folders
  lapply(c("raw", "pre", "post", "metadata", "r data", "taxo"),
         FUN = function(x){dir.create(paste(input.folder, x, sep =""), 
                                      showWarnings = F)})
  # create pre & post data folders 
  lapply(X = c("raw", "pre", "post"), f = c("csv", meta.vars),
         FUN = function(x, f){
           if(x == "raw"){f <- c(f, "metadata","taxo")}
           lapply(f, FUN = function(f, x){
             dir.create(paste(input.folder, x, "/", f, sep =""),
                        showWarnings = F)}, 
             x = x)})
  
  # migrate folder function
  migrate_folder <- function(folder, remove.f = F) {
    
    files <- list.files(paste(input.folder, folder, sep =""), full.names = T)[
      grep(".csv", list.files(paste(input.folder, folder, sep ="")))]
    
    
    file.copy(from = files, 
              to = paste(input.folder, "pre/", gsub("pre_", "", folder),"/", sep =""), 
              recursive = F, overwrite = F, 
              copy.mode = TRUE, copy.date = TRUE)
    
    if(remove.f){
      file.remove(folder) 
    }
  }
  
  # migrate from old folders
  if(migrate == T){
    f = c("csv", meta.vars)
    lapply(f, migrate_folder, remove.f = F)
  }
}

#' Initialise database
#'
#' @param var.vars 
#' @param match.vars 
#' @param meta.vars 
#' @param taxo.vars 
#' @param spp.list_src 
#'
#' @return
#' @export
#'
#' @examples
init_db <- function(var.vars = c("var", "value", "data.ID"),
                    match.vars = c("synonyms", "data.status"),
                    meta.vars = c("qc", "observer", "ref", "n", "notes"),
                    taxo.vars = c("genus", "family", "order"),
                    spp.list_src = NULL) {
  
  master.vars <- c("species", match.vars, var.vars, meta.vars)
  
  cat("configuring: ", ls(), sep = "\n")
  master_config = setNames(lapply(ls(), get, envir = environment()), 
                           ls()[!ls() %in% "master_config"])
  while("master_config" %in% search()){
    detach(master_config)
  }
  attach(master_config, 2)
  return()
}
#' Ensure ncodes
#'
#' Generate a valid named vector of fs folders. Names represent fcodes (folder codes), used 
#' to effect data processing across file.system.
#' @param meta.vars caharacter vector of meta.var names
#'
#' @return
#' @export
#'
#' @examples
ensure_fcodes <- function(meta.vars){
  
  reserved <- c("csv", "ref", "n")
  i <- 1
  f <- c("csv", meta.vars)
  
  if(any(!reserved %in% f)){stop(reserved[!reserved %in% f], "missing from meta.vars")}
  
  names(f) <- substr(f, 1, i)
  names(f)[f == "csv"] <- "d"
  f <- c(f[f %in% reserved],f[!f %in% reserved])
  while(any(duplicated(names(f)))){
        names(f)[duplicated(names(f))] <- substr(f[duplicated(names(f))], 1, i + 1)
  }
  names(f) <- toupper(names(f))
  return(f)
}

#' Title
#'
#' @param path path from 'input.folder/pre/' to file.
#' @param fcodes 
#' @param file.names 
#'
#' @return
#' @export
#'
#' @examples
path_to_dcode <- function(path, fcodes, file.names) {
  s.path <- unlist(strsplit(path, "/"))
  paste(names(fcodes)[s.path[1] == fcodes], 
        gsub("x", "", names(file.names)[s.path[2] == file.names]), sep = "")
}

#' Create named file.names vector
#'
#' @param file.names 
#' @param trim.to.existing 
#' @param from.data_log 
#'
#' @return
#' @export
#'
#' @examples
create_file.names <- function(file.names = NULL) {
  
  if(any(!file.exists(paste(input.folder, "pre/csv/", file.names, sep = "")))){
    bpaths <- paste(input.folder, "pre/csv/", file.names, sep = "")[!file.exists(
      paste(input.folder, "pre/csv/", file.names, sep = ""))]
    stop("file.names supplied do not correspond to valid files in file.system. \n",
         "Paths returning error: \n'", paste(bpaths, collapse = "'\n'"), "'")
  }
  
  if(!file.exists(paste(input.folder, "metadata/data_log.csv", sep = ""))){
    warning("data_log.csv not found at: \n '", input.folder, "metadata/data_log.csv'",
            "Ensure data_log is created from file.names vector in next step")
    
    if(is.null(file.names)){message("no file.name information supplied.",
                                    "Extracting valid file.names from file.system",
                                    "\n NOTE: generic dcodes assigned to file.names alphabetically \n")
      file.names <- list.files(paste(input.folder, "pre/csv", sep = ""), recursive = T) %>%
        grep(pattern = "Icon\r", inv=T, value=T)
    }
    file.names <- setNames(file.names, paste("x", 0:(length(file.names)-1), sep = ""))
    return(file.names)   
  }
  
  data_log <- read.csv(paste(input.folder, "metadata/data_log.csv", sep = ""),
                       stringsAsFactors = F, strip.white = T, 
                       na.strings = c("NA", "-999", "", " "), 
                       blank.lines.skip = TRUE)
  
  file.names.dl <- setNames(data_log[, "file.name"], gsub("D", "x", data_log[,"dcode"]))
  if(!is.null(file.names)){
    file.names <- file.names.dl[file.names.dl %in% file.names]}else{
      file.names <- file.names.dl
    }
  
  cat("dcodes succesfully extracted from 'data_log.csv'")
  return(file.names)
}


#' Create data_log.csv
#'
#' Creates a data.frame and writes it to data_log.path
#' @param fcodes vector of file.system folder names. named with fcodes
#' @param file.names vector of file.names to include in data_log. If unnamed, 
#' file.names are coded alphabetically. Otherwise should be named generic with the 
#' dcode to be associated with each file.name.
#' @param overwrite whether to overwrite any existing file specified by data_log.path
#' @param data_log.path path to write data_log.csv to. defaults to path: 
#' 'metadata/data_log.csv' in the file.system
#'
#' @return a data_log data.frame
#' @export
#'
#' @examples
create_data_log <- function(fcodes = fcodes, file.names, overwrite = F,
                            data_log.path = NULL){
  if(is.null(data_log.path)){
    data_log.path <- paste(input.folder, "metadata/data_log.csv", sep = "")}
  if(!overwrite & file.exists(data_log.path)){
    warning("data_log.csv already exists. Use overwrite = T to overwrite \n existing loaded")
    return(read.csv(file = paste(input.folder, "metadata/data_log.csv", sep = "")))
    }
  
  dl.names <- c("dcode", "file.name", "descr", "source", "source.contact", "method", "notes")
  fs <- list.files(paste(input.folder, "pre/csv/", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  
  if(length(file.names) == 1){
    if(file.names == "blank"){
      data_log <- setNames(data.frame(matrix(nrow = 0, ncol = length(dl.names))), dl.names) 
      write.csv(data_log, file = data_log.path,
                row.names = F)
      cat(paste("blank data_log written to: \n'", data_log.path, "'\n", "Complete to continue",
                sep = ""))
      return(NULL)
    }
    
    if(file.names == "fromFS"){
      file.names <- fs %>% setNames(paste("D", 0:(length(fs)-1), sep = ""))
      data_log <- data.frame(matrix("", nrow = length(file.names), ncol = length(dl.names))) %>%
        setNames(dl.names)
      data_log[,"dcode"] <- names(file.names)
      data_log[,"file.name"] <- file.names
      write.csv(data_log, file = paste(input.folder, "metadata/data_log.csv", sep = ""),
                row.names = F)
      return(data_log)
    }
  }
  
  if(!is.vector(file.names)){stop("non vector argument supplied to 'file.names' where character vector expected")}
  
  if(any(!file.names %in% fs)){stop("file.names: ", file.names[!file.names %in% fs],
                                    ", not valid file.name in 'csv/' folder")}
  if(is.null(names(file.names))){
    file.names <- setNames(file.name, paste("D", 0:(length(fs)-1), sep = ""))}
  
  data_log <- data.frame(matrix("", nrow = length(file.names), ncol = length(dl.names))) %>%
    setNames(dl.names)
  data_log[,"dcode"] <- gsub("x", "D", names(file.names))
  data_log[,"file.name"] <- file.names
  data_log <- data_log[order(data_log$dcode),]
  write.csv(data_log, file = paste(input.folder, "metadata/data_log.csv", sep = ""),
            row.names = F)
  return(data_log)
}

#' Create vnames.csv
#'
#' @param file.names a vector of file.names for which columns should be created in 
#' vnames.csv. If NULL, function attempts to extract file.names from data_log.
#' @param data_log a data_log data.frame
#' @param input.folder project input.folder path. Defaults to input.folder in 
#' global environment
#' @param fcodes 
#' @param overwrite overwrite whether to overwrite any existing file specified 
#' by vnames.path
#' @param vnames.path 
#'
#' @return
#' @export
#'
#' @examples
create_vnames <- function(file.names = NULL, data_log = NULL,  
                          fcodes = fcodes, overwrite = F, vnames.path = NULL) {
  if(is.null(vnames.path)){vnames.path <- paste(input.folder, "metadata/vnames.csv", 
                                                sep = "")}
  if(file.exists(vnames.path) & !overwrite){
    stop("vnames.csv already exists. Use overwrite = T to overwrite")
  }
  
  if(is.null(file.names)){print("no files.names provided. Checking for data_log")
    if(is.null(data_log)){
      if(file.exists(paste(input.folder, "metadata/data_log.csv", sep = ""))){
        print("extracting file.names from data_log")
        file.names <- create_file.names(file.names = NULL, trim.to.existing = F, 
                 from.data_log = T, input.folder = input.folder)  
      }else{stop("no data_log supplied")}}}
  
  if(is.null(names(file.names))){
    stop("file.names not named. Vector of file.names named with generic dcodes required")}
  
  fs <- list.files(paste(input.folder, "pre", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  dcodes <- sapply(fs, FUN = path_to_dcode, fcodes, file.names)
  
    vnames <- data.frame(matrix(nrow = 0, ncol = length(dcodes))) %>% setNames(dcodes)
    vnames <- vnames[,order(names(vnames))]
    print(paste("writing vnames.cvs to path:",  "'",vnames.path,"'", sep = ""))
    write.csv(vnames, vnames.path, row.names = T, na = "")

  }
 

#' Load system reference files
#'
#' @param view 
#' @param na.strings 
#' @param fileEncoding 
#'
#' @return
#' @export
#'
#' @examples
load_sys.ref <- function(view = F, na.strings=c("","NA", " ", "-999"), 
                         fileEncoding = "") {
 
  if(!file.exists(paste(input.folder, "metadata/","metadata.csv", sep = ""))){
    stop("no metadata.csv in ", paste(input.folder, "metadata/", sep = ""))
  }
  metadata <- read.csv(paste(input.folder, "metadata/","metadata.csv", sep = ""), 
                       stringsAsFactors = F, fileEncoding = fileEncoding, 
                       na.strings=na.strings, strip.white = T, 
                       blank.lines.skip = T) 
  
  if(!file.exists(paste(input.folder, "metadata/","data_log.csv", sep = ""))){
    stop("no data_log.csv in ", paste(input.folder, "metadata/", sep = ""))
  }  
  data_log <- read.csv(paste(input.folder, "metadata/","data_log.csv", sep = ""), 
                       stringsAsFactors = F, fileEncoding = fileEncoding, 
                       na.strings=na.strings, strip.white = T, 
                       blank.lines.skip = T)

  if(!file.exists(paste(input.folder, "metadata/","vnames.csv", sep = ""))){
    stop("no vnames.csv in ", paste(input.folder, "metadata/", sep = ""))
  }
  vnames <- read.csv(paste(input.folder, "metadata/","vnames.csv", sep = ""), 
                     stringsAsFactors = F, fileEncoding = fileEncoding, 
                     na.strings=na.strings, strip.white = T, 
                     blank.lines.skip = T)
  sys.files <- c("metadata", "data_log", "vnames")
  cat("loading: ", sys.files, sep = "\n")
  cat("attaching to env: sys.ref")
  sys.ref = setNames(lapply(sys.files, get, envir = environment()), 
                     sys.files)
  while("sys.ref" %in% search()){
    detach(sys.ref)
  }
  attach(sys.ref, 3)
  
  
  if(view){
    View(metadata)
    View(data_log)
    View(vnames)
  }
  return()
}


#' Check vnames against file.names
#'
#' @param file.names 
#' @param vnames.path 
#' @param input.folder 
#' @param fcodes 
#' @param data_log 
#'
#' @return
#' @export
#'
#' @examples
check_vnames <- function(file.names = file.names, vnames.path = NULL,
                          fcodes = fcodes, data_log = NULL) {
  
  if(is.null(vnames.path)){vnames.path <- paste(input.folder, "metadata/vnames.csv", sep = "")}
  
  if(is.null(file.names)){print("no files.names provided. Checking for data_log")
    if(is.null(data_log)){
      if(file.exists(paste(input.folder, "metadata/data_log.csv", sep = ""))){
        print("extracting file.names from data_log")
        file.names <- create_file.names(file.names = NULL, trim.to.existing = F, 
                                     from.data_log = T , input.folder = input.folder)  
      }else{stop("no data_log supplied. Can't get dcode info")}}}
  
  if(is.null(names(file.names))){
    stop("file.names vector not named. Vector of file.names named with generic dcodes required")}
  
  fs <- list.files(paste(input.folder, "pre", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T) 

  fs <- unique(grep(paste(file.names,collapse="|"), 
                          fs, value=TRUE))
  
  dcodes <- sapply(fs, FUN = path_to_dcode, fcodes, file.names)

  
  if(!file.exists(vnames.path)){stop("path to vnames.csv not valid")}
  vnames <- read.csv(vnames.path, strip.white = T,
           stringsAsFactors = T, na.strings = c("NA", "", " "))

  
  if(all(dcodes %in% names(vnames))){
    print("all files in file system have valid vnames columns")}else{
      stop("no valid 'vnames.csv' columns for files:","\n ", paste(dcodes[dcodes %in% names(vnames)],
                            names(dcodes)[dcodes %in% names(vnames)], 
                            sep = ":", collapse = "\n "))
    }
  }
  
  

#' Title
#'
#' @param file.name 
#' @param file.name.out 
#' @param na.strings 
#' @param fileEncoding 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
process_csv <- function(file.name, file.name.out = file.name, 
                        file.names = file.names, fcode = fcodes["D"], 
                        na.strings = c("NA", "", " ", "-999"), 
                        fileEncoding = "", ...) {
  
  vname.col <- gsub("x", names(fcode), names(file.names)[file.names == file.name])
  cat("===================================================================\n")
  cat(paste("processing", file.name, vname.col, "\n"))
  
  data<- read.csv(paste(input.folder, "pre/", fcodes[fcodes == fcode],"/", file.name, sep = ""), 
                  header = T, strip.white = T, stringsAsFactors = F,
                  na.strings = na.strings, blank.lines.skip = TRUE,
                  fileEncoding = fileEncoding)
  
  if(exists("var.omit")){
  data <- data[,!names(data) %in% var.omit, drop = F]}
  
  if(file.exists(paste(script.folder, "process/", gsub(".csv", "", file.name), 
                       ".R", sep = ""))){
    source(paste(script.folder, "process/", gsub(".csv", "", file.name), 
                 ".R", sep = ""), local = T)
    cat(paste("sourced: process/", gsub(".csv", "", file.name), 
                ".R", "\n", sep = ""))
    }
  
  if(file.exists(paste(script.folder, "process/", gsub(".csv", "", file.name), "_",
                       fcode, ".R", sep = ""))){
    source(paste(script.folder, "process/", gsub(".csv", "", file.name), "_",
                 fcode, ".R", sep = ""), local = T)
    cat(paste("sourced: process/", gsub(".csv", "", file.name), "_",
                fcode,".R", "\n", sep = ""))}
  
  #This is for the brain size database that has problems with variable names
  vnames[, vname.col] <- gsub(" ", ".", vnames[, vname.col])
  vnames[, vname.col] <- gsub("\\(", ".", vnames[, vname.col])
  vnames[, vname.col] <- gsub("\\)", ".", vnames[, vname.col])
  
  
  #For brain size
  keep.vars <- na.omit(vnames[, vname.col])
  if(!all(keep.vars %in% names(data))){
    stop(paste("no column match in .csv for", vname.col, "vnames:", 
               keep.vars[which(!keep.vars %in% names(data))]))
  }
  
  data<- data[,keep.vars]
  
  names(data) <- vnames$code[match(names(data), vnames[, vname.col])]
  
  if("genus" %in% names(data)){
    data$species <- paste(data$genus, "_", data$species, sep = "")
    data <- data[, names(data) != "genus"]
    cat("'genus_species' combined in species column. Genus data removed", "\n")
  }
  
  if(anyDuplicated(data$species) > 0){
    message("duplicate species name in ",  vname.col)
  }
  
  if(any(is.na(data$species))){
    data <- data[!is.na(data$species),]
    warning("NAs detected in species column, data set: ", vname.col,". \n", 
            sum(is.na(data$species))," rows removed")
  }
  
  
  cat(paste("df", c("nrow (species)", "ncol (traits)"),":", dim(data)), "\n")
  
  write.csv(data, paste(input.folder, "post/", fcodes[fcodes == fcode], "/", file.name, sep = ""),
            row.names = F)
  cat("***", "\n")
  
}


#' Process files in the file.system
#'
#' @param file.names can be "fromFS" to extract file.names from contents of folder "pre/" 
#' or a vector of file.names named with dcode (both generic and specific handled).
#' @param fcodes character vector of file.system folder names. Named with appropriate fcode.
#'
#' @return
#' @export
#'
#' @examples
process_file.system <- function(file.names, fcodes) {
  
  check_vnames(file.names = file.names, vnames.path = NULL,
               fcodes = fcodes)
  
  if(length(file.names) == 1 & all(file.names == "fromFS")){
    file.names <- create_file.names(from.data_log = T)
  }
  
  process.l <- lapply(fcodes, FUN = function(x) {
    f.files <- list.files(paste(input.folder, "pre/", x, sep = "")) %>% 
      grep(pattern = "Icon\r", invert = T, value = T)
    f.files <- f.files[f.files %in% file.names]}) 
  process.l <-process.l[sapply(process.l, FUN = function(x) {length(x) != 0})]
  
  for(fcode in names(process.l)){
    for(file.name in process.l[[fcode]]){
      process_csv(file.name, file.names = file.names, fcode = fcodes[fcode], 
                  fileEncoding = "mac", file.name.out = file.name, vnames = vnames) 
    }
  }
}


#' Convert long data.frame to master format
#'
#' Function accepts long data.frame, checks columns and converts to master format 
#' @param data long data.frame
#' @param master.vars 
#' @param data.ID 
#'
#' @return
#' @export
#'
#' @examples
longtoMasterFormat <- function(data = NULL, master.vars, spp.list = NULL,
                               file.name){
  if(is.null(data)){
    if(is.null(file.name)){stop("no data or file.name supplied")}
    stopifnot(!is.null(names(file.name)))
    df <- read.csv(paste(input.folder, "post/csv/", file.name))
  }
  
  df <- newMasterData(master.vars, nrow = dim(data)[1])
  
  keep <- names(data)[names(df) %in% master.vars]
  
  df[match(keep, names(df))] <- data[,keep]
  df$synonyms <- df$species
  df$data.status <- "original"
  df$data.ID <- names(file.name)
  attr(df, "format") <- "master"
  
  output <- list(data = df, spp.list = spp.list, file.name = file.name)
  attr(output, "type") <- "data:master"
  attr(output, "status") <- "unmatched"
  
  
  return(df)
}

#' Create [[master]]
#'
#' @param spp.list 
#' @param data 
#' @param file.name 
#'
#' @return
#' @export
#'
#' @examples
create_master <- function(spp.list, data = NULL, file.name) {
  
  if(is.null(data)){data <- newMasterData(master.vars)
  file.names <- NULL
  }else{
    if(attr(data, "format") != "master"){
      stop("data not data.frame with attr:format == master")
      }
    
    if(!file.exists(paste(input.folder, "post/csv/", file.name, sep =""))){
      stop("data file.name supplied returning invalid pathway: \n", 
           paste(input.folder, "post/csv/", file.name, sep =""))
      }
    }
  
  
  master <- list(data = newMasterData(master.vars), 
                 spp.list = spp.list, 
                 metadata = metadata)
  
  attr(master, "type") <- "master"
  attr(master, "file.names") <- "empty"
  
  return(master)
}


# create spp.list and add taxonomic data
createSpp.list <- function(species, taxo.dat = NULL, taxon.levels = NULL, 
                           spp.list_src = NULL){

  if(!is.null(spp.list_src)){
    if(!exists("file.names", 1)){
      stop("dcode named file.names vector required in global environment to use spp.list_src for spp.list")}
    
    if(!file.exists(paste(input.folder, "post/csv/", file.names[gsub("D", "x", spp.list_src)], sep = ""))){
     stop("spp.list_src: ", spp.list_src, " is returning invalid pathway: \n", 
          paste(input.folder, "post/csv/", file.names[gsub("D", "x", spp.list_src)], sep = ""))}
    
    species <- unique(read.csv(paste(input.folder, "post/csv/", file.names[gsub("D", "x", spp.list_src)], sep = ""), 
             header = T)$species)
    cat("species list extracted from dataset: ", spp.list_src, "\n file.name: ",
              file.names[gsub("D", "x", spp.list_src)], sep = "")
  }
  
  if(is.null(species)){stop("no species supplied")
  }
  species <- trimws(gsub(" ", "_", species))
  
  if(is.null(taxo.dat)){
    spp.list <- data.frame(species = species, master.spp = T, rel.spp = NA, 
                                   taxo.status = "original")
    attr(spp.list, "type") <- "spp.list"
    return(spp.list)}
  
  if(!is.null(taxo.dat)){
  if(any(!species %in% taxo.dat$species)){
    print(species[!species %in% taxo.dat$species])
    stop("species data missing in taxo.dat")}
  
    if(is.null(taxon.levels)){taxon.levels <- names(taxo.dat)}
  if(any(!taxo.vars %in% names(taxo.dat))){print(taxo.vars[!taxo.vars %in% names(taxo.dat)])
    stop("taxo.vars data missing in taxo.dat")}
  
  taxo.dat <- taxo.dat[taxo.dat$species == species, taxo.vars]
  }
  
  spp.list <- data.frame(species = species, master.spp = T, rel.spp = NA, 
                         taxo.status = "original", taxo.dat)
  
  attr(spp.list, "type") <- "spp.list"
  return(spp.list)
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
  attr(meta, "type") <- "meta"
  
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



#' Check meta.df species against data.
#'
#'Check species in a meta.df match data species. Re-order and trim meta.df if necessary
#' @param meta.df meta.var data.frame
#' @param data data data.frame
#' @param meta.var character string indicating meta.var
#'
#' @return a data.frame with nrow = nrow(data) and with rows in same species order as data data.frame
#' @export
#'
#' @examples
checkMetaSpecies <- function(meta.df, data, meta.var){
  
  # check
  if(any(!data$species %in% meta.df$species)){
    if(meta.var == "ref"){
      stop("no match in ", meta.var,  " for species in data:",
           data$species[!data$species %in% meta.df$species])}else{
             warning("no match in ", meta.var,  " for species in data:",
                     data$species[!data$species %in% meta.df$species])}}
  
  # reorder
  new.meta.df <- data.frame(matrix(NA, nrow(data), ncol(meta.df)))
  d_in_m <- !is.na(match(data$species, meta.df$species))
  new.meta.df[d_in_m,] <- meta.df[na.omit(match(data$species, meta.df$species)),]
  names(new.meta.df) <- names(meta.df)
  
  return(new.meta.df)
  
}  



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
compileMeta <- function(m, input.folder = NULL, fileEncoding = "", all.override = F){
  
  meta <- m$meta
  data <- m$data
  
  for(meta.var in names(meta)){
    
    print("============================================================================")
    print(paste("processing meta.var:", meta.var))
    meta.df <- meta[[meta.var]]
    
    # expand single value to `all` column
    if(is.vector(meta.df)){
      if(length(meta.df) > 1){
        warning(meta.var, " argument supplied as vector has length > 1. Only first element used")
        meta.df <- meta.df[1]}
      meta.df <- data.frame(species = data$species, all = meta.df)}
    
    if(is.data.frame(meta.df)){
      meta.df <- checkMetaSpecies(meta.df, data, meta.var)
    }
    
    
    if(m$file.name %in% list.files(paste(input.folder, "post/", meta.var, "/", sep =""))){
      print(paste("loading ", "'", meta.var, "/", m$file.name, "'", sep = ""))
      
      load.df <- read.csv(paste(input.folder, "post/", meta.var, "/", m$file.name,sep = ""), 
                          stringsAsFactors = F, fileEncoding = fileEncoding) %>%
        checkMetaSpecies(data, meta.var)
      
      if(is.null(meta.df)){meta.df <- load.df}else{cbind(meta.df <- meta.df, load.df)}}
    
    if(!is.null(meta.df)){
    # trim to single column for `all` if all.override = T
    if(all.override & "all" %in% names(meta.df)){
      meta[[meta.var]] <- meta.df[,c("species", "all")]}
    
    if(any(duplicated(names(meta.df)))){
      stop("duplicated trait columns supplied to meta.var: ", meta.var)}     
    
    # MATCHING METADATA COLUMNS TO DATA COLUMNS
    # vector of data vars to check for metadata
    check.vars <- names(data)[!(names(data) %in% "species")]
    
    # if there are metadata for all data variables, return check.vars metadata
    if(all(check.vars %in% names(meta.df))){
      meta[[meta.var]] <- meta.df[,c("species", check.vars)]}else{
        
        # if not all data vars have metadata matches, it is likely that meta.df 
        # columns contain data associated w/ multiple data columns. Information should 
        # be provided in a meta.var group look up table. check whether it exists.
        if(file.exists(paste(input.folder, "post/", meta.var, "/", 
                             gsub(".csv", "",m$file.name), "_", meta.var, "_group", ".csv",
                             sep =""))){
          # load csv in which meta group names are assigned to individual data variables
          meta.grp.df  <- read.csv(paste(input.folder, "post/", meta.var, "/", 
                                         gsub(".csv", "",m$file.name), "_", 
                                         meta.var, "_group.csv", sep = ""), 
                                   stringsAsFactors = F)
        }else{
          # if group look up table does not exist, create csv in which individual 
          # data variables can be assigned to meta group names. If data variable 
          # has no metadata, leave as NA in group.
          meta.grp.df <- data.frame(var = check.vars, grp = NA)
          meta.grp.df$grp[meta.grp.df$var %in% names(meta.df)] <- meta.grp.df$var[meta.grp.df$var %in% names(meta.df)]
          write.csv(meta.grp.df, paste(input.folder, "post/", meta.var, "/",
                                       gsub(".csv", "", m$file.name), "_", meta.var, "_group.csv", 
                                       sep = ""),
                    row.names = F)
          stop(paste(meta.var,".group.csv created, in post/",
                     meta.var," folder. Update file to proceed", 
                     sep = ""))}
        
        # Check that all variables are assigned to valid meta data column or NA in the case 
        # of no data. Stop if not.
        if(!all(na.omit(meta.grp.df$grp) %in% names(meta.df))){
          stop(paste("assigned meta group names does not match supplied meta data names, update _",
                     meta.var,".group file to proceed", sep = ""))}
        
        
        # Make sure ALL variables have reference data
        if(meta.var == "ref" & any(is.na(meta.grp.df$grp))){
          stop(paste("variables missing reference column. update ", 
                     meta.var,".group file to proceed", sep = ""))
        }
        
        # Isolate variables to be assigned meta data. Create new dataframe containing the 
        # appropriate meta column for each variable. 
        # Name with data variables and update appropriate meta slot
        check.vars <- meta.grp.df$var[which(!is.na(meta.grp.df$grp))]
        dd <- data.frame(species = data$species, matrix(NA, nrow = dim(data)[1], 
                                                        ncol = length(check.vars)))
        names(dd) <- c("species", check.vars)
        dd[,check.vars] <- meta.df[match(dd$species, meta.df$species),
                                   meta.grp.df$grp[match(check.vars, 
                                                         meta.grp.df$var)]]
        print(paste(meta.var, " vars matched successfully to ", "post/", 
                    meta.var, "/", gsub(".csv", "",m$file.name), "_", 
                          meta.var, "_group.csv", sep = ""))
        meta[[meta.var]] <- dd
      }}
    
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


#' Compile to master database long format
#' 
#' Compiles dataset into format compatible with appending to master database. 
#' Takes match object m.
#' @param m 
#' @param meta.vars 
#' @param match.vars 
#' @param var.vars 
#'
#' @return
#' @export
#'
#' @examples
masterDataFormat <-  function(m, meta.vars, match.vars, var.vars){
  
  master.vars <- c("species", match.vars, var.vars, meta.vars)
    
  data <- m$data
  
  #make vector of data variables to be added
  data.vars <- names(data)[!names(data) %in% c("species", match.vars)]
  data.dat <- data.frame(data[, data.vars])
  names(data.dat) <- data.vars
  
  #find non NA values in match data. Match arr.indices to spp and variable names (for QA)
  id <- which(!is.na(data.dat), arr.ind = T)
  species <- as.character(data[,"species"][id[, "row"]])
  var <- as.character(data.vars[id[, "col"]])
  data.ID <- m$data.ID
  value <- data.dat[id]
  
  
  mdat <- data.frame(matrix(NA, ncol = length(master.vars), nrow = length(species)))
  names(mdat) <- master.vars
  
  for(var.var in c("species", var.vars)){
    
    mdat[,var.var] <- get(var.var)}
  
  
  for(match.var in match.vars){
    
    mdat[,match.var] <- data[id[,"row"], match.var]}
  
  for(meta.var in meta.vars){
    
    mdat[,meta.var] <- getMeta(meta.var, meta = m$meta, spp = species, var = var)}
     
  if(any(is.na(mdat$ref))){               
  warning(sum(is.na(mdat$ref)), " data points missing reference information!:",
      "\n (", format((sum(is.na(mdat$ref))/nrow(mdat))*100, digits = 2), 
      "% of ", nrow(mdat),")", sep = "")}
  
  attr(mdat, "format") <- "master"
  output <- list(data = mdat, spp.list = m$spp.list, 
                 file.name = setNames(m$file.name, m$data.ID))
  attr(output, "type") <- "data:master"
  attr(output, "status") <- "matched"
  
    return(output)
}


updateMaster <- function(master, output){
  
  data <- output$data
  spp.list <- output$spp.list
  
  if(!all(data$species %in% spp.list$species)){
    print(data$species[!data$species %in% spp.list$species])
      stop("data and spp.list species name mismatch")}
    
  if(!all(unique(data$var) %in% master$metadata$code)){
    print(unique(data$var)[!unique(data$var) %in% master$metadata$code])
    stop("missing variable metadata for data vars")
  }
  

  if(all(names(master$data) == names(data))){
  master$data <- rbind(master$data, data)
  master$spp.list <- spp.list
  attr(master, "file.names") <- c(attr(master, "file.names"), output$file.name) %>% 
    grep(pattern = "empty", invert = T, value = T)
  return(master)}else{
    stop("update data format error. column name mismatch")
  }
  
}
  
newMasterData <- function(master.vars, nrow = NULL){
  if(is.null(nrow)){nrow <- 0}
  data <- data.frame(matrix(vector(), nrow, length(master.vars),
                      dimnames = list(c(), master.vars)))
  attr(data, "format") <- "master"
  return(data)
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
              taxo.vars <- names(spp.list)[!names(spp.list) %in% c("species",
                                                                   "rel.spp",
                                                                   "master.spp", 
                                                                   "taxo.status")]
              
              spp.list <- rbind(spp.list, data.frame(species = spp, master.spp = F, 
                                                     rel.spp = syns[1], taxo.status = "copied", 
                                                     spp.list[syns[1], taxo.vars]))}
          }
        
      }
    }
    
    match.dd <- rbind(match.dd, match)
  }
  
  if(is.null(match.dd)){}else{
    add.dd <- cbind(match.dd, data[match(match.dd$synonyms, data$species), 
                                   !names(data) %in% c("species", "synonyms"),
                                   drop = F])
    add.dd$data.status <- "modified"
    data <- rbind(data, add.dd)}
  
  m$data <- data
  m$spp.list <- spp.list
  
  return(m)}


# match data set to master species list using all available known match pair tables.
dataSppMatch <- function(m, syn.links = syn.links, 
                         addSpp = T, ignore.unmatched = T){
  
  sub <- m$sub
  set <- m$set
  
  # Check whether matching required and match
  unmatched <- m[[sub]]$species[!(m[[sub]]$species %in% m[[set]]$species)]
  if(length(unmatched) == 0){print(paste(m$data.ID, "data direct match to spp.list, no further matching required"))
    attr(m, "status") <- "full_match"
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
  
  # if no more species unmatched break loop
  if(length(unmatched) == 0){
    print(paste(m$data.ID, "match complete"))
    attr(m, "status") <- "full_match"
    # Trim data
    m$data <- m$data[m$data$species %in% m$spp.list$species,]
  }else{
    warning(paste("match incomplete,",length(unmatched), sub, "datapoints unmatched"))
    attr(m, "status") <- paste("matched:incomplete - ", length(unmatched), " ", m$sub,
                               " unmatched", sep = "")
    m$unmatched <- data.frame(species = unmatched, synonyms = NA)
    
    if(ignore.unmatched){
      # Trim data
      m$data <- m$data[m$data$species %in% m$spp.list$species,]
    }else{
      stop(paste("manual match required to continue"))
    }
  }
  

  attr(m$data, "format") <- "data:wide"
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
  
  if("data.cre" %in% names(data)){
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
matchObj <- function(data.ID = NULL, spp.list, data = NULL,  
                     sub, meta, file.name = NULL, unmatched = NULL, fileEncoding = ""){
  
  if(is.null(file.name)){stop("required argument file.name not supplied")}
  
  if(!file.exists(paste(input.folder, "post/csv/", file.name, sep = ""))){
    stop("file.name specifying invalid path: \n \"", 
         paste(input.folder, "post/csv/", file.name, sep = ""),
         "\"")}
  
  if(is.null(data.ID)) {
    data.ID <- gsub("x", "D", names(file.names)[file.names == file.name])}
  
  if(is.null(data)) {
   data <-  read.csv(paste(input.folder, "post/csv/", file.name, sep = ""),
             stringsAsFactors=FALSE, fileEncoding = fileEncoding)}
  
  if(sub == "spp.list"){
    set <- "data"
  }
  if(sub == "data"){
    set <- "spp.list"
  }   
  
  attr(data, "format") <- "data:wide"
  
  m <- list(data.ID = data.ID, spp.list = spp.list, data = data, sub = sub, 
            set = set, meta = meta, file.name = file.name)
  attr(m, "type") <- "match.object"
  attr(m, "status") <- "unmatched"
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
testSynonym <- function(syn, m){
  
  NAs <- which(is.na(m$unmatched$synonyms))
  
  if(length(NAs) == 0){
    print("no unmatched species")
    return(m)
  }
  
  spp <- m$unmatched$species[min(NAs)]
  syn <- gsub(" ","_", syn)
  match <- any(syn %in% c(m[[m$set]]$species, "Extinct","New"))
  
  print(paste("unmatched species:", spp,"matched to", syn, "?"))
  print(match)
  
  if(match){
    
    m$unmatched$synonyms[m$unmatched$species == spp] <- syn
    NAs <- which(is.na(m$unmatched$synonyms))
    if(length(NAs) == 0){
      print("no unmatched species")
    }else{
      spp <- m$unmatched$species[min(NAs)]
      print(paste("next unmatched:", spp))
    }
  }
  
  return(m)
}




whichNext <- function(m){
  
  NAs <- which(is.na(m$unmatched$synonyms))
  
  if(length(NAs) == 0){
    print("no unmatched species")
    return(m)
  } else {
    spp <- m$unmatched$species[min(NAs)]
    print(paste("next unmatched", spp)) 
  }
}




#' Title
#'
#' @param data 
#' @param outliers 
#'
#' @return
#' @export
#'
#' @examples
removeData <- function(data = master$data, outliers = outliers){
  
  remove <- NULL
  for(i in 1:dim(outliers)[1]){
    
    remove <- c(remove, which(data$species == outliers[i, "species"] &
                                data$var == outliers[i, "var"] & data$data.ID == outliers[i, "data.ID"],
                              arr.ind = T))
  }
  
  data <- data[-remove,]
  
  return(data)
}




#' numerise
#'
#' converts vector to numeric if all elements coercible
#' @param x vector whose elements can be coerced to numeric
#'
#' @return if x coercible to numeric returns vector x
#' with elements converted to numeric. If non-coercible returns x
#' @export
#'
#' @examples
numerise <- function(x){if(all(grepl('^[0-9.]+$', x))) as.numeric(x) else x}



#' Title
#'
#' @param someColor 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

# matches variable names to codes
#' Title
#'
#' @param dat 
#' @param data.ID 
#' @param metadata 
#' @param vnames 
#'
#' @return
#' @export
#'
#' @examples
codeVars <- function(dat, data.ID, metadata = metadata, vnames = vnames){
  # code new variables added to dataset using metadata table
  names(dat)[match(metadata$orig.vname[which(metadata$orig.vname %in% names(dat))], names(dat))] <- 
    metadata$code[which(metadata$orig.vname %in% names(dat))]
  
  # code variable names which match directly to variables in master using vnames tables
  names(dat)[match(vnames[,data.ID][which(vnames[,data.ID] %in% names(dat))], names(dat))] <- 
    vnames$code[which(vnames[,data.ID] %in% names(dat))]  
  
  if(any(is.na(names(dat)))){stop("error in coding variable. No matching code found")}
  
  return(dat)}


