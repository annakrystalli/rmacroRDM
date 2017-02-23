if (!require("pacman")) install.packages("pacman")
pacman::p_load(c("dplyr", "plyr", "readr", "listenv"), character.only = T)


#' Setup file.system.
#' 
#' Sets up file.system with correct folder structure
#' 
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

setupFileSystem <- function(migrate = F, ...){
  
  dir.create(paste(ds$data.folder, "inputs", sep = ""), showWarnings = F)
  dir.create(paste(ds$data.folder, "outputs", sep = ""), showWarnings = F)
  dir.create(paste(ds$data.folder, "info", sep = ""), showWarnings = F)
  dir.create(paste(ds$data.folder, "inputs/data", sep = ""), showWarnings = F)
  
  dir.create(paste(ds$script.folder, "process", sep = ""), showWarnings = F)
  
  dir.create(paste(ds$output.folder, "data", sep = ""), showWarnings = F)
  dir.create(paste(ds$output.folder, "reports", sep = ""), showWarnings = F)
  dir.create(paste(ds$output.folder, "figures", sep = ""), showWarnings = F)
  
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

setupInputFolder <- function(migrate = F){
  
  if(!file.exists(ds$input.folder)){stop("invalid input.folder path")}
  if(!exists("meta.vars", envir = ds)){
    stop("meta.vars not configured. \n ensure data.base is initialised: see init_db()")
  }
  
  # create process script folder
  dir.create(paste0(ds$script.folder, "process/"), 
             showWarnings = F)
  
  
  if(substr(ds$input.folder, nchar(ds$input.folder), nchar(ds$input.folder)) != "/"){
    ds$input.folder <- paste(ds$input.folder, "/", sep = "")
  }
  
  # create data folders
  lapply(c("raw", "pre", "post", "metadata", "r data", "taxo"),
         FUN = function(x){dir.create(paste(input.folder, x, sep =""), 
                                      showWarnings = F)})
  # create pre & post data folders 
  lapply(X = c("raw", "pre", "post"), f = c("csv", ds$meta.vars),
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
    f = c("csv", ds$meta.vars)
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
init_db <- function(script.folder = getwd(), 
                    data.folder = NULL,
                    var.vars = c("var", "value", "dcode"),
                    match.vars = c("synonyms", "data.status"),
                    meta.vars = c("qc", "observer", "ref", "n", "notes"),
                    taxo.vars = c("genus", "family", "order", "class"),
                    metadata.vars = c("code", "orig.vname", "cat", "descr", 
                                      "scores", "levels", "type", "units", "notes", "source", 
                                      "log"),
                    
                    data_log.vars =  c("dcode", "format","descr", "source", "source.contact", 
                                       "contact.details", "method", "notes"),
                    spp.list_src = NULL,
                    na.strings=c("","NA", " ", "-999"), 
                    fileEncoding = "UTF-8",
                    envir = .GlobalEnv,
                    return.list = F) {
  
  if(any(is.null(c(data.folder, script.folder)))){
    stop(paste("no", 
               c(data.folder, script.folder)[is.null(c(data.folder, script.folder))]))
  }else{
    
    input.folder <- paste(data.folder, "inputs/data/", sep = "")
    output.folder <- paste(data.folder, "outputs/", sep = "")
  }
  master.vars <- c("species", match.vars, var.vars, meta.vars)
  data_log.path <- paste(input.folder, "metadata/data_log.csv", sep = "")
  metadata.path <- paste(input.folder, "metadata/metadata.csv", sep = "")
  vnames.path <- paste(input.folder, "metadata/vnames.csv", sep = "")
  fileEncodings.path <- paste(input.folder, "metadata/fileEncodings.rda", sep = "")
  fcodes <- ensure_fcodes(meta.vars)
  ignore <- c("obs", "envir", "return.list", "ignore")
  
  cat("configuring: ", ls(), sep = "\n")
  obj = setNames(lapply(ls()[!ls() %in% ignore], 
                        get, envir = environment(), inherits = F), 
                 ls()[!ls() %in% ignore])
  if(return.list){return(as.listenv(obj))}
  while("ds" %in% search()){
    detach(ds)
  }
  ds <- new.env(parent = emptyenv())
  list2env(obj, envir = ds)
  assign("ds", ds, envir = envir)
}
#' Ensure ncodes
#'
#' Generate a valid named vector of fs folders. Names represent fcodes (folder codes), used 
#' to effect data processing across file.system.
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
#'
#' @return
#' @export
#'
#' @examples
path_to_dcode <- function(path) {
  s.path <- unlist(strsplit(path, "/"))
  fcode <- ds$fcodes[s.path[1] == ds$fcodes]
  data_log <- read.csv(ds$data_log.path, 
                       stringsAsFactors = F, fileEncoding = "",
                       na.strings = ds$na.strings, strip.white = T, 
                       blank.lines.skip = T, header = T)
  
  file.name.row <- which(data_log[, paste(fcode, "_file.name", sep = "")] == s.path[2])
  if(length(file.name.row) == 0){
    stop("path: '", path, "' does not match a valid ", fcode, "_file.name entry in data_log")
  }
  
  paste(names(fcode), 
        gsub("D", "", 
             data_log$dcode[file.name.row]), 
        sep = "")
}

#' Create named file.paths vector
#'
#' @param dcodes 
#' @param file.names 
#'
#' @return
#' @export
#'
#' @examples
get_file.paths <- function(dcodes = NULL, file.names = NULL) {
  
  file.paths <- list.files(paste(ds$input.folder, "pre/", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  file.paths <- setNames(file.paths, sapply(file.paths, FUN = path_to_dcode))
  
  if(is.null(dcodes)){
    if(is.null(file.names)){
      return(file.paths)
    }else{
      dcodes <- sr$data_log$dcode[sr$data_log$csv_file.name %in% file.names]
    }
  }
  
  data_log.sub <- sr$data_log[sr$data_log$dcode %in% dcodes, , drop = F]
  paths.sub <- data_log2path(data_log.sub)
  file.paths <- file.paths[file.paths %in% paths.sub]
  return(file.paths)
}

dl_name2fcode <- function(dl_names){
  ds$fcodes[ds$fcodes %in% gsub("_file.name", "", dl_names)]
}

data_log2path <- function(data_log = sr$data_log){
  files <- data_log[ ,grep("file.name", names(data_log))]
  path.fcodes <- dl_name2fcode(names(files))
  ids <- which(!is.na(files), arr.ind = T)
  paste(path.fcodes[ids[,2]], files[!is.na(files)], sep = "/")
}

get_file.names <- function() {
  
  file.names <- list.files(paste(ds$input.folder, "pre/csv/", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  
  file.names <- setNames(file.names, 
                         sapply(paste("csv", file.names, sep = "/"), 
                                FUN = path_to_dcode))
  return(file.names)   
}

#' Title
#'
#' @param overwrite 
#'
#' @return
#' @export
#'
#' @examples
create_metadata <- function(save = F){
  metadata <- setNames(data.frame(matrix(0,nrow=0,ncol=length(ds$metadata.vars))), 
                       ds$metadata.vars)
  if(save){
    print(paste("writing metadata.cvs to path:",  "'", ds$metadata.path,"'", sep = ""))
    write.csv(metadata, file = ds$metadata.path, row.names = F)
  }  
}

usr_file <- function(rel.path = "metadata/metadata.csv", usr) {
  
  
}

#' Create data_log.csv
#'
#' Creates a data.frame and writes it to data_log.path
#' @param file.names vector of file.names to include in data_log. If unnamed, 
#' file.names are coded alphabetically. Otherwise should be named generic with the 
#' dcode to be associated with each file.name.
#' @param overwrite whether to overwrite any existing file specified by data_log.path
#'
#' @return a data_log data.frame
#' @export
#'
#' @examples
create_data_log <- function(file.order, save = T){
  
  dl.metavars <- list.files(paste(ds$input.folder, "pre/", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T) %>% strsplit("/") %>%
    sapply(FUN = function(x){x[1]}) %>% unique()
  dl.metavars <- dl.metavars[order(match(dl.metavars, ds$fcodes))]
  
  dl.names <- c(ds$data_log.vars[1], paste(dl.metavars, "_file.name", sep = ""), 
                ds$data_log.vars[2:length(ds$data_log.vars)])
  
  fs <- list.files(paste(ds$input.folder, "pre/csv/", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  
  if(length(file.order) == 1 & file.order == "blank"){
    data_log <- setNames(data.frame(matrix(nrow = 0, ncol = length(dl.names))), dl.names)
    if(save){
      write.csv(data_log, file = ds$data_log.path,
                row.names = F)
      cat(paste("blank data_log written to: \n'", ds$data_log.path, "'\n", "Complete to continue",
                sep = ""))
    }
    return()
  }
  
  if(!is.nulll(file.order)){
    if(!setequal(file.order, fs)){
      stop("file.order file names do not match file names in 'pre/csv/'")
    }else{fs <- fs[match(fs, file.order)]}
  }
  file.order.fs <- fs %>% setNames(paste("D", 0:(length(fs)-1), sep = ""))
  data_log <- data.frame(matrix("", nrow = length(file.order.fs), ncol = length(dl.names))) %>%
    setNames(dl.names)
  data_log[,"dcode"] <- names(file.order.fs)
  data_log[,"csv_file.name"] <- file.order.fs
  if(save){
    write.csv(data_log, file = ds$data_log.path,
              row.names = F)
  }
  return()
  
}

#' Create vnames.csv
#'
#' @param overwrite overwrite whether to overwrite any existing file specified 
#' by vnames.path
#'
#' @return
#' @export
#'
#' @examples
create_vnames <- function(save = F) {
  
  fs <- list.files(paste(ds$input.folder, "pre", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  dcodes <- sapply(fs, FUN = path_to_dcode)
  
  vnames <- data.frame(matrix(nrow = 0, ncol = length(dcodes))) %>% setNames(dcodes)
  vnames <- vnames[,order(names(vnames))]
  if(save){
    print(paste("writing vnames.cvs to path:",  "'",ds$vnames.path,"'", sep = ""))
    write.csv(vnames, ds$vnames.path, row.names = T, na = "", 
              fileEncoding = sr$fileEncodings["vnames", "encoding"])
  }
}



update_data_log <- function(save = F, trim2fs = FALSE, envir = .GlobalEnv) {
  if(!file.exists(ds$data_log.path)){stop("no valid data_log found or specified")}
  
  data_log <- read.csv(file = ds$data_log.path,
                       stringsAsFactors = F, fileEncoding = sr$fileEncodings["data_log", "encoding"], 
                       na.strings = ds$na.strings, strip.white = T, 
                       blank.lines.skip = T)
  
  dl.metavars <- list.files(paste(ds$input.folder, "pre/", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T) %>% strsplit("/") %>%
    sapply(FUN = function(x){x[1]}) %>% unique()
  dl.metavars <- dl.metavars[order(match(dl.metavars, ds$fcodes))]
  
  dl.names <- c(ds$data_log.vars[1], paste(dl.metavars, "_file.name", sep = ""), 
                ds$data_log.vars[2:length(ds$data_log.vars)])
  
  dl.names <- c("dcode", paste(dl.metavars, "_file.name", sep = ""), "format",
                "descr", "source", "source.contact", "contact.details", "method", "notes")
  
  if(!setequal(names(data_log), dl.names)){
    data_log_tmp <- data.frame(matrix(NA, nrow = nrow(data_log), ncol = length(dl.names))) %>%
      setNames(dl.names)
    data_log <- data_log[, names(data_log) %in% dl.names]
    data_log_tmp[, names(data_log)] <- data_log
    data_log <- data_log_tmp
  }
  
  fs_all <- list.files(paste(ds$input.folder, "pre", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  fs_csv <- list.files(paste(ds$input.folder, "pre/csv", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  
  if(any(!fs_csv %in% data_log$csv_file.name)){
    add_csv <- setdiff(fs_csv, data_log$csv_file.name)
    data_log_add <- data.frame(matrix(NA, nrow = length(add_csv), ncol = length(dl.names))) %>%
      setNames(dl.names)
    data_log_add$file.name <- add_csv
    data_log_add$dcode <- paste("D", (nrow(data_log) + 1):(nrow(data_log) + length(add_csv)),
                                sep = "")
    data_log <- rbind(data_log, data_log_add)
  }
  if(trim2fs){
    data_log <- data_log[data_log$csv_file.name %in% fs_csv,]
  }
  
  
  if(save){
    print(paste("writing updated data_log to: ", 
                ds$data_log.path, sep = ""))
    write.csv(data_log, file = ds$data_log.path,
              row.names = F, fileEncoding = sr$fileEncodings["data_log",
                                                             "encoding"])
    if(exists("sr", envir = envir)){
      if(exists("data_log", envir = sr)){
        sr$data_log <- data_log
      }
    }
  }
  return(data_log)
  
  
}

#' Order a vector of dcodes
#'
#' @param dcodes 
#'
#' @return
#' @export
#'
#' @examples
order_dcodes <- function(dcodes) {
  dcode_names <- expand.grid(names(ds$fcodes), 0:max(as.numeric(substring(dcodes, 2)))) %>%
    apply(1, paste, collapse="") 
  dcode_index <- setNames(1:length(dcode_names), dcode_names)
  dcode_order <- dcode_index[dcodes]
  dcodes <- dcodes[order(dcode_order)]
  return(dcodes)
}


#' validate fs path
#'
#' Takes a file.system path to a file in the pre/ folder.
#'
#' @return
#' @export
#'
#' @examples
validate_data_log <- function(force.stop = F, return.files = F){
  fs <- list.files(paste(ds$input.folder, "pre", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  
  dl.cols <- names(sr$data_log)[grep("_file.name", names(sr$data_log))]
  fs.fold <- gsub("_file.name", "/", dl.cols)
  dl.files <- sr$data_log[, dl.cols][!is.na(sr$data_log[, dl.cols])]
  dl.fs <- paste0(fs.fold[which(!is.na(sr$data_log[, dl.cols]), arr.ind = T)[,2]], dl.files)
  if(any(!dl.fs %in% fs)){
    if(force.stop == T){
      stop("file.names in data_log with no matching file in file.system: \n \n", 
           paste0(dl.fs[!dl.fs %in% fs], collapse = "\n")) 
    }
    if(force.stop == F){
      warning("file.names in data_log with no matching file in file.system: \n \n", 
              paste0(dl.fs[!dl.fs %in% fs], collapse = "\n"))
      if(return.files){return(dl.fs[!dl.fs %in% fs])}else{return(FALSE)}
    }
  }else{if(return.files){return(NULL)}else{return(TRUE)}}
}

#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
validate_path <- function(path){
  s.path <- unlist(strsplit(path, "/"))
  s.path[2] %in% sr$data_log[,paste0(s.path[1], "_file.name")]
}

#' Title
#'
#' @param overwrite 
#'
#' @return
#' @export
#'
#' @examples
update_vnames <- function(save = F, envir = .GlobalEnv){
  if(!all(exists("sr", envir = envir), exists("data_log", sr))){
    stop("data_log not loaded into sr")
    }
  
  fs <- list.files(paste(ds$input.folder, "pre", sep = ""), recursive = T) %>%
    grep(pattern = "Icon\r", inv=T, value=T)
  if(!all(sapply(fs, FUN = validate_path))){
    warning("files in file.system with no match in data_log: \n \n", 
            paste0(fs[sapply(fs, FUN = validate_path)], collapse = "\n"))
  }
  
  valid.dl <- validate_data_log()
  dcodes <- sapply(fs, FUN = path_to_dcode) %>% order_dcodes()
  
  vn.cols <- c("code", "meta", dcodes)
  vn.rows <- unique(c(ds$master.vars, ds$taxo.vars, sr$metadata$code))
  vnames_tmp <- data.frame(matrix(NA, nrow = length(vn.rows), ncol = length(vn.cols)))
  names(vnames_tmp) <- vn.cols
  rownames(vnames_tmp) <- vn.rows
  vnames_tmp$code <- vn.rows
  
  tsf.cols <- intersect(names(sr$vnames), names(vnames_tmp))  
  vnames_tmp[match(sr$vnames$code, rownames(vnames_tmp)), tsf.cols] <- sr$vnames[,tsf.cols]
  
  if(save){
    print(paste("writing updated vnames to: ", 
                ds$vnames.path, sep = ""))
    write.csv(vnames_tmp, file = ds$vnames.path,
              row.names = F, fileEncoding = sr$fileEncodings["vnames",
                                                             "encoding"])
    if(exists("sr", envir = envir)){
      if(exists("vnames", envir = sr)){
        sr$vnames <- vnames_tmp
      }
    }
  }
  return(vnames_tmp)
}

#' Load system reference files
#'
#' @param view 
#'
#' @return
#' @export
#'
#' @examples
load_sys.ref <- function(view = F, sys.files = NULL, envir = .GlobalEnv,
                         return.list = F) {
  if(is.null(sys.files)){
    sys.files <- c("metadata", "data_log", "vnames", "fileEncodings")
  }
  
  if(!file.exists(ds$fileEncodings.path)){
    error("no fileEncodings file in ", paste(ds$input.folder, "metadata/", sep = ""), 
          " yet. Use guess_fileEncodings")
  }else{  
    load(ds$fileEncodings.path)
    if(view){
      View(fileEncodings)
    }
  }
  
  
  if("metadata" %in% sys.files){
    if(!file.exists(ds$metadata.path)){
      stop("no metadata.csv in ", paste(ds$input.folder, "metadata/", sep = ""))
    }
    metadata <- read.csv(ds$metadata.path, 
                         stringsAsFactors = F, fileEncoding = fileEncodings["metadata",
                                                                            "encoding"],
                         na.strings = ds$na.strings, strip.white = T, 
                         blank.lines.skip = T, header = T)
    if(view){
      View(metadata)
    }
  }
  
  if("data_log" %in% sys.files){
    if(!file.exists(ds$data_log.path)){
      stop("no data_log.csv in ", paste(ds$input.folder, "metadata/", sep = ""))
    }  
    data_log <- read.csv(ds$data_log.path, 
                         stringsAsFactors = F, fileEncoding = fileEncodings["data_log",
                                                                            "encoding"],
                         na.strings = ds$na.strings, strip.white = T, 
                         blank.lines.skip = T, header = T)
    if(view){
      View(data_log)
    }
  }
  
  if("vnames" %in% sys.files){
    if(!file.exists(ds$vnames.path)){
      stop("no vnames.csv in ", paste(ds$input.folder, "metadata/", sep = ""))
    }
    vnames <- read.csv(ds$vnames.path, 
                       stringsAsFactors = F, fileEncoding = fileEncodings["vnames",
                                                                          "encoding"],
                       na.strings = ds$na.strings, strip.white = T, 
                       blank.lines.skip = T, header = T)
    if(view){
      View(vnames)
    }
  }
  
  cat("loading: ", sys.files, sep = "\n")
  obj = setNames(lapply(sys.files, get, envir = environment(), inherits = F), 
                 sys.files)
  
  if(return.list){return(as.listenv(obj))}
  cat("attaching to env: sr")
  if(all(c("metadata", "data_log", "vnames", "fileEncodings") %in% sys.files)){
    sr <- new.env(parent = emptyenv())
    list2env(obj, envir = sr)
    assign("sr", sr, envir = envir)
  }else{
    if(!exists("sr", envir)){
      sr <- new.env(parent = emptyenv())
      list2env(obj, envir = sr)
      assign("sr", sr, envir = envir)
    }else{
      assign(names(obj), obj, env = sr)}
  }
  
  return()
}


#' Check vnames against file.names
#'
#' @param file.names 
#' @param dcodes 
#'
#' @return
#' @export
#'
#' @examples
check_vnames <- function(dcodes = NULL, file.names = NULL) {
  
  file.paths <- get_file.paths(dcodes, file.names)
  dcodes <- names(file.paths)
  
  if(all(dcodes %in% names(sr$vnames))){
    print("all files in file system have valid vnames columns")}else{
      stop("no valid 'vnames.csv' columns for files:","\n ", 
           paste(dcodes[dcodes %in% names(sr$vnames)],
                 file.paths[dcodes %in% names(sr$vnames)], 
                 sep = ":", collapse = "\n "))
    }
}



#' Title
#'
#' @param file.path 
#' @param save.taxo 
#'
#' @return
#' @export
#'
#' @examples
process_csv <- function(file.path, save.taxo = F) {
  
  dcode <- path_to_dcode(file.path)
  file.name <- strsplit(file.path, "/")[[1]][2]
  format <- sr$data_log[sr$data_log$dcode ==  gsub("[[:alpha:]]", "D", dcode), "format"]
  
  cat("===================================================================\n")
  cat(paste("processing", file.path, dcode, "\n"))
  
  data <- read.csv(paste(ds$input.folder, "pre/", file.path, sep = ""), 
                   header = T, strip.white = T, stringsAsFactors = F,
                   na.strings = ds$na.strings, blank.lines.skip = TRUE,
                   fileEncoding = sr$fileEncodings[dcode,"encoding"], check.names = T)
  
  data <- data[,!sapply(data, function(x)all(is.na(x)))]
  add_keep.vars <- NULL
  if(format == "long"){
    add_keep.dat <- data[add_keep.vars, , drop = F]
  }
  if(format == "wide"){
    add_keep.dat <- data[ , add_keep.vars, drop = F]
  }
  
  # source _pre custom processing script
  if(file.exists(paste(ds$script.folder, "/process/", gsub(".csv", "", file.name), "_pre",
                       ".R", sep = ""))){
    source(paste(ds$script.folder, "/process/", gsub(".csv", "", file.name),  "_pre",
                 ".R", sep = ""), local = T)
    cat(paste("sourced: process/", gsub(".csv", "", file.name),  "_pre",
              ".R", "\n", sep = ""))
  }
  
  keep.vars <- make.names(na.omit(sr$vnames[, dcode]))
  
  if(format == "wide"){
    data.vars <- names(data)}
  if(format == "long"){
    var.col <- sr$vnames[sr$vnames$code == "var", dcode]
    data.vars <- names(data)
    variable.vars <- unique(make.names(data[,var.col]))
    keep.vars <- setdiff(keep.vars, variable.vars)
    data <- data[!is.na(data[,var.col]),]
    if(!all(variable.vars %in% make.names(data[,var.col]))){
      stop(paste("no column match in .csv for", dcode, "vnames:", 
                 variable.vars[which(!variable.vars %in% make.names(data[,var.col]))]))
    }
  }
  
  if(!all(keep.vars %in% data.vars)){
    stop(paste("no column match in .csv for", dcode, "vnames:", 
               keep.vars[which(!keep.vars %in% names(data))]))
  }
  
  data <- data[,keep.vars]
  names(data) <- sr$vnames$code[match(names(data), make.names(sr$vnames[, dcode]))]
  if(format == "long"){
    if(any(is.na(sr$vnames$code[match(make.names(data[,"var"]), 
                                      make.names(sr$vnames[, dcode]))]))){
      stop("names in ", dcode, " var column do not match entries in vnames: \n \n",
           is.na(sr$vnames$code[match(make.names(data[,"var"]), 
                                      make.names(sr$vnames[, dcode]))]))
    }
    data[,"var"] <- sr$vnames$code[match(make.names(data[,"var"]), 
                                         make.names(sr$vnames[, dcode]))]
    add_keep.dat <- add_keep.dat[,keep.vars]
    data <- rbind(data, add_keep.dat) 
  }
  if(format == "wide"){
    data <- cbind(data, add_keep.dat)
  }
  
  # ---- standardise species ----
  firstup <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  data$species <- gsub("[[:punct:]]", "_", data$species)
  data$species <- gsub(" ", "_", data$species)
  
  if(length(grep("_", data$species)) == 0){
    if(!"genus" %in% names(data)){
      stop("check ", dcode, " species info.\n",
           "Valid 'genus_species' format cannot be extracted from 'species' 
           column and no 'genus' column supplied")}
    data$species <- paste(data$genus, "_", data$species, sep = "")
    cat("'genus_species' combined in species column.", "\n")
  }
  data$species <- firstup(data$species)
  
  #---- separate and save taxonomic data ----
  if(save.taxo & (dcode %in% names(get_file.names()))){
    taxo <- data[, names(data) %in% c("species", ds$taxo.vars)]
    write.csv(taxo, file = paste0(ds$input.folder, "taxo/", dcode,"_taxo.csv", sep = ""), 
              row.names = F, fileEncoding = sr$fileEncodings[dcode, "encoding"])
    cat("taxonomic vars: [", names(data)[names(data) %in% c("species", ds$taxo.vars)],  
        "] saved as \n'" ,paste0(ds$input.folder, "taxo/", dcode,"_taxo.csv", sep = ""),  "'\n")
  }
  # ---- remove taxo.vars ----
  data <- data[,!names(data) %in% ds$taxo.vars]
  
  if(anyDuplicated(data$species) > 0 & format == "wide"){
    message("duplicate species name in ",  dcode)
  }
  
  if(any(is.na(data$species))){
    data <- data[!is.na(data$species),]
    warning("NAs detected in species column, data set: ", dcode,". \n", 
            sum(is.na(data$species))," rows removed")
  }
  
  if(file.exists(paste(ds$script.folder, "/process/", gsub(".csv", "", file.name), "_post",
                       ".R", sep = ""))){
    source(paste(ds$script.folder, "/process/", gsub(".csv", "", file.name),  "_post",
                 ".R", sep = ""), local = T)
    cat(paste("sourced: process/", gsub(".csv", "", file.name),  "_post",
              ".R", "\n", sep = ""))
  }
  
  if(format == "wide"){
    dims <- dim(data)
    dims[2] <- dims[2]-1 
  }
  if(format == "long"){
    dims <- c(length(unique(data$species)), length(unique(data$var)))
  }
  cat(paste(c("species", "traits"),":", dims), "\n")
  
  write.csv(data, paste(ds$input.folder, "post/", file.path, sep = ""),
            row.names = F, fileEncoding = sr$fileEncodings[dcode, "encoding"])
  cat("***", "\n")
  
}


#' Process files in the file.system
#'
#' @param file.names can be "fromFS" to extract file.names from contents of folder "pre/" 
#' or a vector of file.names named with dcode (both generic and specific handled).
#'
#' @return
#' @export
#'
#' @examples
process_file.system <- function(dcodes = NULL, file.names = NULL, save.taxo = F) {
  
  check_vnames(dcodes, file.names)
  
  file.paths <- get_file.paths(dcodes = dcodes, file.names = file.names)
  
  for(file.path in file.paths){
    process_csv(file.path, save.taxo = save.taxo) 
  }
}


#' Convert long data.frame to master format
#'
#' Function accepts long data.frame, checks columns and converts to master format 
#' @param data long data.frame
#' @param dcode 
#'
#' @return
#' @export
#'
#' @examples
longtoMasterFormat <- function(file.name = get_file.names()[ds$spp.list_src], 
                               spp.list = NULL){
  if(is.null(names(file.name))){
    dcode <- names(get_file.names()[get_file.names() == file.name])}else{
      dcode <- names(file.name)
    }
  data <- read.csv(paste0(ds$input.folder, "post/csv/", file.name),
                   stringsAsFactors = F, fileEncoding = sr$fileEncodings[dcode, "encoding"],
                   na.strings = ds$na.strings, strip.white = T, 
                   blank.lines.skip = T, header = T)
  
  
  df <- newMasterData(nrow = dim(data)[1])
  
  
  keep <- names(data)[names(data) %in% ds$master.vars]
  
  df[match(keep, names(df))] <- data[,keep]
  df$synonyms <- df$species
  df$data.status <- "original"
  df$dcode <- dcode
  attr(df, "type") <- "data:master"
  attr(df, "status") <- "unmatched"
  
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
create_master <- function(file.name = NULL, data = NULL, spp.list = NULL) {
  
  if(is.null(data)){
    if(is.null(file.name)){data <- newMasterData()}else{
      if(is.null(names(file.name))){
        names(file.name) <- names(get_file.names()[get_file.names() == file.name])}
      if(sr$data_log[sr$data_log$dcode == names(file.name), "format"] == "long"){
        data <- longtoMasterFormat(file.name)
      }
    }
  }else{
    if(attr(data, "format") != "master"){
      stop("data not data.frame with attr:format == master")
    }
  }
  
  master <- list(data = data, 
                 spp.list = spp.list, 
                 metadata = sr$metadata)
  
  attr(master, "type") <- "master"
  attr(master, "file.names") <- file.name
  
  return(master)
}


# create spp.list and add taxonomic data
createSpp.list <- function(species = NULL, taxo.dat = NULL, spp.list_src = ds$spp.list_src, 
                           sources = NULL, syn_links = NULL){
  
  if(!is.null(spp.list_src)){
    spp.list_src.path <- get_file.paths()[spp.list_src]
    if(!file.exists(paste0(ds$input.folder, "post/", spp.list_src.path))){
      stop("spp.list_src: ", spp.list_src, " is returning invalid pathway: \n", 
           paste0(ds$input.folder, "post/", spp.list_src.path))}
    
    species <- unique(read.csv(paste0(ds$input.folder, "post/", spp.list_src.path), 
                               header = T, stringsAsFactors = F, fileEncoding = [ds$spp.list_src, "encoding"], 
                               na.strings = ds$na.strings, strip.white = T, 
                               blank.lines.skip = T)$species)
    cat("species list extracted from dataset: ", spp.list_src, "\n file.name: ",
        spp.list_src.path, sep = "")
  }
  
  if(is.null(species)){stop("no species supplied")}
  
  species <- trimws(gsub(" ", "_", species))
  
  # create spp.list
  spp.list <- data.frame(species = species, master.spp = T, rel.spp = NA, 
                         taxo.status = "original")

  # source taxonomic info from `spp.list_src`_taxo.csv if exists
  if(is.null(taxo.dat)){
    if(!is.null(spp.list_src) & file.exists(paste0(ds$input.folder,
                                                      "taxo/", spp.list_src,
                                                      "_taxo.csv"))){
      taxo.dat <- read.csv(paste0(ds$input.folder, "taxo/", spp.list_src, "_taxo.csv"))
    }}
  if(!is.null(taxo.dat)){
    if(any(!species %in% taxo.dat$species)){
      print(species[!species %in% taxo.dat$species])
      stop("species data missing in taxo.dat")}
    taxo.dat <- taxo.dat[match(spp.list$species, taxo.dat$species),]
    taxo.dat <- taxo.dat[, names(taxo.dat) %in% c(ds$taxo.vars), drop = F]
    spp.list <- cbind(spp.list, taxo.dat)
  }
  
  attr(spp.list, "type") <- "spp.list"
  attr(spp.list, "spp.list_src") <- spp.list_src
  attr(syn_links, "sources") <- sources
  attr(spp.list, "syn_links") <- syn_links
  
  return(spp.list)}








#' Create metadata list
#' 
#' Creates a named metadata list of the appropriate length to match supplied vector of meta.vars
#' @keywords meta 
#' @export
#' @return named list the same length as meta.vars.
#' @examples
#' createMeta()

createMeta <- function(){
  
  meta <- vector("list", length(ds$meta.vars))
  names(meta) <- ds$meta.vars
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
#' @param fileEnconding character string: if non-empty declares the encoding used on a file (not a connection) 
#' so the character data can be re-encoded. Defaults to NULL
#' @keywords meta
#' @return updated meta list 
#' @export
#' @examples
#' compileMeta()
#' 
compileMeta <- function(m, all.override = F){
  
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
    mcode <- gsub("D", names(ds$fcodes)[ds$fcodes == meta.var], m$dcode)
    m.path <- get_file.paths()[mcode]
    if(file.exists(paste0(ds$input.folder, "post/",m.path))){
      print(paste("loading ", "'", m.path, "'", sep = ""))
      
      load.df <- read.csv(paste(ds$input.folder, "post/", m.path,sep = ""), 
                          stringsAsFactors = F, fileEncoding = sr$fileEncodings[m$dcode, "encoding"],
                          na.strings = ds$na.strings, strip.white = T, 
                          blank.lines.skip = T, header = T) %>%
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
          if(file.exists(paste(ds$input.folder, "post/", meta.var, "/", 
                               gsub(".csv", "",m$file.name), "_", meta.var, "_group", ".csv",
                               sep =""))){
            # load csv in which meta group names are assigned to individual data variables
            meta.grp.df  <- read.csv(paste(ds$input.folder, "post/", meta.var, "/", 
                                           gsub(".csv", "",m$file.name), "_", 
                                           meta.var, "_group.csv", sep = ""), 
                                     stringsAsFactors = F, fileEncoding = sr$fileEncodings[m$dcode, "encoding"],
                                     na.strings = ds$na.strings, strip.white = T, 
                                     blank.lines.skip = T, header = T)
          }else{
            # if group look up table does not exist, create csv in which individual 
            # data variables can be assigned to meta group names. If data variable 
            # has no metadata, leave as NA in group.
            meta.grp.df <- data.frame(var = check.vars, grp = NA)
            meta.grp.df$grp[meta.grp.df$var %in% names(meta.df)] <- meta.grp.df$var[meta.grp.df$var %in% names(meta.df)]
            write.csv(meta.grp.df, paste(ds$input.folder, "post/", meta.var, "/",
                                         gsub(".csv", "", m$file.name), "_", meta.var, "_group.csv", 
                                         sep = ""),
                      row.names = F, fileEncoding = sr$fileEncodings[m$dcode, "encoding"])
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
#' @param m match object0
#' @keywords meta
#' @return if metadata complete, returns m. if not, produces error.
#' @export
#' @examples
#' checkVarMeta()
checkVarMeta <- function(m){
  
  if(!all(names(m$data) %in% c(sr$metadata$code, "species"))){
    print(names(m$data)[!(names(m$data) %in% c(sr$metadata$code, "species"))])
    stop("metadata missing for some variables, metadata file needs updating")
  }else{
    print(paste(m$dcode, "metadata complete"))
    return(m)
  }
}





#' Compile to master database long format
#' 
#' Compiles dataset into format compatible with appending to master database. 
#' Takes match object m.
#' @param m 
#'
#' @return
#' @export
#'
#' @examples
masterDataFormat <-  function(m){
  
  data <- m$data
  
  #make vector of data variables to be added
  data.vars <- names(data)[!names(data) %in% c("species", ds$match.vars)]
  data.dat <- data.frame(data[, data.vars])
  names(data.dat) <- data.vars
  
  #find non NA values in match data. Match arr.indices to spp and variable names (for QA)
  id <- which(!is.na(data.dat), arr.ind = T)
  species <- as.character(data[,"species"][id[, "row"]])
  var <- as.character(data.vars[id[, "col"]])
  dcode <- m$dcode
  value <- data.dat[id]
  
  
  mdat <- data.frame(matrix(NA, ncol = length(ds$master.vars), nrow = length(species)))
  names(mdat) <- ds$master.vars
  
  for(var.var in c("species", ds$var.vars)){
    
    mdat[,var.var] <- get(var.var)}
  
  
  for(match.var in ds$match.vars){
    
    mdat[,match.var] <- data[id[,"row"], match.var]}
  
  for(meta.var in ds$meta.vars){
    
    mdat[,meta.var] <- getMeta(meta.var, meta = m$meta, spp = species, var = var)}
  
  if(any(is.na(mdat$ref))){               
    warning(sum(is.na(mdat$ref)), " data points missing reference information!:",
            "\n (", format((sum(is.na(mdat$ref))/nrow(mdat))*100, digits = 2), 
            "% of ", nrow(mdat),")", sep = "")}
  
  attr(mdat, "format") <- "master"
  output <- list(data = mdat, spp.list = m$spp.list, 
                 file.name = setNames(m$file.name, m$dcode))
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

newMasterData <- function(nrow = NULL){
  if(is.null(nrow)){nrow <- 0}
  data <- data.frame(matrix(vector(), nrow, length(ds$master.vars),
                            dimnames = list(c(), ds$master.vars)))
  attr(data, "format") <- "master"
  return(data)
}


# look up unmatched species in table (lookup.dat) of known match pairs. 
# If list is of unmatched data species (ie dataset species is a subset of spp.list), 
# match to known synonyms. If list is of unmatched spp.list species (ie spp.list a subset of 
# data$species), match to known species. 
sppMatch <- function(m, unmatched = unmatched, addSpp = T){
  
  data <- m$data
  spp.list <- m$spp.list
  syn_links <- attr(spp.list, "syn_links")
  sub <- m$sub
  set <- m$set
  
  match.dd <- NULL
  
  for(spp in unmatched){
    
    syns <- getAllSyns(syn_links, spp)
    
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
  attr(spp.list, "syn_links") <- syn_links
  m$data <- data
  m$spp.list <- spp.list
  
  return(m)}


# match data set to master species list using all available known match pair tables.
dataSppMatch <- function(m, addSpp = T, ignore.unmatched = T){
  
  sub <- m$sub
  set <- m$set
  
  # Check whether matching required and match
  unmatched <- m[[sub]]$species[!(m[[sub]]$species %in% m[[set]]$species)]
  if(length(unmatched) == 0){print(paste(m$dcode, "data direct match to spp.list, no further matching required"))
    attr(m, "status") <- "full_match"
    return(m)}
  
  # remove extinct or new species from data to be added
  if(sub == "data"){
    rm <- c(synSets(attr(m$spp.list, "syn_links"), spp = "Extinct"), 
            synSets(attr(m$spp.list, "syn_links"), spp = "New"))
    m$data <- m$data[!(m$data$species %in% rm),]
  }
  
  m <- sppMatch(m, unmatched = unmatched, addSpp = addSpp)
  
  #generate next unmatched species list
  unmatched <- m[[sub]]$species[!(m[[sub]]$species %in% m[[set]]$species)]
  
  # if no more species unmatched break loop
  if(length(unmatched) == 0){
    print(paste(m$dcode, "match complete"))
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

#' Title
#'
#' @param file.paths 
#' @param save 
#' @param n_max 
#'
#' @return
#' @export
#'
#' @examples
guess_fileEncodings <- function(file.paths = NULL,                      
                                file.dcodes = NULL, save = F, 
                                n_max = 10000, envir = .GlobalEnv){
  if(is.null(file.paths)){
    file.names <- c(paste0(ds$input.folder,"pre/", get_file.paths()),
                    ds$metadata.path, ds$data_log.path, ds$vnames.path)
    file.dcodes <- c(names(get_file.paths()), "metadata", "data_log", "vnames")
  }else{
    if(is.null(file.dcodes)){
      stop("supply docde for files")
    }
    stopifnot(length(file.paths) == length(file.dcodes))
    file.names <- paste0(ds$input.folder,file.paths)
    names(file.names) <- file.dcodes
  }
  
  fileEncodings <- llply(file.names, guess_encoding, n_max = n_max, threshold = 0.2, 
                         .progress = "text") %>% lapply(function(x){x[1,]}) %>% 
    t() %>% do.call(what = rbind) %>% data.frame()
  fileEncodings$encoding <- as.character(fileEncodings$encoding)
  fileEncodings$assessment <- "auto"
  rownames(fileEncodings) <- file.dcodes
  
  if(any(is.na(fileEncodings$encoding))){
    fileEncodings$encoding[is.na(fileEncodings$encoding)] <- ""}
  if(save){
    save(fileEncodings, file = ds$fileEncodings.path)
  }
  return(fileEncodings)
}

update_fileEncodings <- function(file.paths = NULL, file.dcodes = NULL,
                                 n_max = 10000, overwrite = F, save = F) {
  load(ds$fileEncodings.path)
  fileEncodings.new <- guess_fileEncodings(file.paths, file.dcodes, save = F)
  outdf <- rbind(fileEncodings, fileEncodings.new[setdiff(rownames(fileEncodings.new), 
                                                          rownames(fileEncodings)),])
  
  if(overwrite){
    if(length(intersect(rownames(fileEncodings.new), rownames(fileEncodings))) != 0){
      overr <- intersect(rownames(fileEncodings.new), rownames(fileEncodings))
      outdf[overr,] <- fileEncodings.new[overr,]
    }
  }
  if(save){
    save(outdf, file = ds$fileEncodings.path)
    if(exists("sr", envir = envir)){
      if(exists("fileEncodings", envir = sr)){
        sr$fileEncodings <- fileEncodings
      }
    }
  }
  return(outdf)
}

# Create match object
matchObj <- function(file.name = NULL, spp.list, sub, dcode = NULL,  data = NULL,  
                     meta = createMeta(),  fileEncoding = "", format = "wide"){
  
  if(is.null(data)) {
    if(is.null(file.name)){stop("required argument file.name not supplied")}
    if(!file.exists(paste(ds$input.folder, "post/csv/", file.name, sep = ""))){
      stop("file.name specifying invalid path: \n \"", 
           paste(ds$input.folder, "post/csv/", file.name, sep = ""),
           "\"")}
    data <-  read.csv(paste(ds$input.folder, "post/csv/", file.name, sep = ""),
                      stringsAsFactors = F, fileEncoding = sr$fileEncodings[dcode, "encoding"],
                      na.strings = ds$na.strings, strip.white = T, 
                      blank.lines.skip = T, header = T)
    format <- sr$data_log[sr$data_log$dcode == dcode, "format"]
    attr(data, "format") <- paste0("data:", format)}
  
    if(is.null(names(file.name))){
      dcode <- names(get_file.names()[get_file.names() == file.name])
    }else{
      dcode <- names(file.name)
    }
  if(sub == "spp.list"){
    set <- "data"
  }
  if(sub == "data"){
    set <- "spp.list"
  }   
  

  
  m <- list(dcode = dcode, data = data, spp.list = spp.list, sub = sub, 
            set = set, meta = meta, file.name = file.name)
  attr(m, "type") <- "match.object"
  attr(m, "status") <- "unmatched"
  return(m)
}


synSets <- function(syn_links, spp){
  
  syns <- unique(unlist(
    syn_links[as.vector(
      unlist(apply(syn_links, 2,
                   FUN = function(x, spp){which(is.element(x, spp), arr.ind = T)},
                   spp))),]))
  
  syns <- syns[!syns %in% spp]
  return(syns)
}

getAllSyns <- function(syn_links, spp){
  
  syna <- synSets(syn_links, spp)
  syns <- syna
  
  while(length(syna) > 0){
    syna <- synSets(syn_links, syns)
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
                                data$var == outliers[i, "var"] & data$dcode == outliers[i, "data.ID"],
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
#' @param dcode 
#' @param metadata 
#' @param vnames 
#'
#' @return
#' @export
#'
#' @examples
codeVars <- function(dat, dcode, metadata = sr$metadata, vnames = sr$vnames){
  # code new variables added to dataset using metadata table
  names(dat)[match(metadata$orig.vname[which(metadata$orig.vname %in% names(dat))], names(dat))] <- 
    metadata$code[which(metadata$orig.vname %in% names(dat))]
  
  # code variable names which match directly to variables in master using vnames tables
  names(dat)[match(vnames[,dcode][which(vnames[,dcode] %in% names(dat))], names(dat))] <- 
    vnames$code[which(vnames[,dcode] %in% names(dat))]  
  
  if(any(is.na(names(dat)))){stop("error in coding variable. No matching code found")}
  
  return(dat)}


#' Update syn_links
#'
#'Update syn_links with manually matched synonyms stored in `m`
#' @param m 
#'
#' @return
#' @export
#'
#' @examples
updateSynlinks <- function(add, spp.list = NULL, species = "species", synonyms = "synonyms", label = NULL) {
  
  if(class(add) == "match.object"){
    add.syns <- add$unmatched[!is.na(add$unmatched$synonyms), , drop = F]
    label < m$dcode
    spp.list <- m$spp.list
    syn_links <- attr(spp.list, "syn_links")
  }else{
    if(is.null(spp.list)){stop("supply valid spp.list")}
    if(all(c(species, synonyms) %in% names(add))){
      add.syns <- setNames(add[, c(synonyms, species)],c("synonyms", "species")) 
    }else{stop("supply valid colnames to `synonyms` and `species` arguments")}
  }
  
  valid <- dim(add.syns)[1] - 
    sum(duplicated(t(apply(rbind(syn_links, add.syns), 1, FUN = sort)))) - 
    sum(duplicated(t(apply(syn_links, 1, FUN = sort))))
  cat(paste(valid, "new syn_links identified \n"))
  
  syn_links <- rbind(syn_links, add.syns)
  syn_links <- syn_links[!duplicated(t(apply(syn_links, 1, FUN = sort))),]
  
  if(valid == 0){cat("no new synonyms to add \n" )}else{
    if(is.null(label)){stop("supply valid `label` argument")}
    attr(syn_links, "sources") <- c(attr(syn_links, "sources"), label)
    cat("`",label, "new `` synonyms added to syn_links \n" )
  }
  attr(spp.list, "syn_links") <- syn_links
  return(spp.list)
  
}


# dtype.over: named vector to override default and assign summarising function for whole data types.
# var.over: named vector to override default and assign summarising function for individual variables. 
# Can also take the name of a prefered dataset to use

widenMaster <- function(master, vars = unique(master$data$var), 
                        species = unique(master$data$species), 
                        add.taxo = F, datSumm = NULL, varSumm = NULL, dupONLY = T){
  
  metadata <- master$metadata
  
  require(dplyr)
  require(tidyr)
  
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  numbIf <- function(X){lapply(X, FUN = function(x) if(is.numeric(t <- type.convert(x))) t else x)}
  
  
  df <- master$data[master$data$species %in% species & master$data$var %in% vars, ]
  #df$value <- trimws(df$value)
  
  # create species x var unique ids and identify duplicate ids
  df <- transform(df, Cluster_ID = as.numeric(interaction(df$species, df$var, drop=TRUE)))
  
  dup.ids <- df$Cluster_ID[duplicated(df[,"Cluster_ID"])]
  
  
  
  # select variable specific datasets to use  
  if(any(unlist(lapply(varSumm, FUN = class)) == "character")){
    vs <- which(unlist(lapply(varSumm, FUN = class)) == "character")
    
    if(dupONLY == F){
      for(i in 1:length(vs)){
        df <- df[!(df$var == names(varSumm)[vs][i] & df$data != varSumm[vs][i]),]
      }}else{
        for(i in 1:length(vs)){
          df <- df[!(df$var == names(varSumm)[vs][i] & df$data != varSumm[vs][i] & df$Cluster_ID %in% dup.ids),]
        }
      }
    
    dup.ids <- df$Cluster_ID[duplicated(df[,"Cluster_ID"])]
    
  }
  
  if(length(dup.ids) != 0){
    
    # create and name default summ function vector
    funs <- c(mean, Mode, Mode, Mode)
    names(funs) <- c("numeric","factor", "logical", "character")
    
    # override summ method according to data type if required
    if(!is.null(datSumm)){funs[names(datSumm)] <- datSumm}
    
    
    # create & split df of duplicate data rows into list containing duplicates
    dup.df <- df[df$Cluster_ID %in% dup.ids,]
    dup.l <- split(dup.df$value, f = dup.df$Cluster_ID)
    cl.ids <- as.numeric(names(dup.l))
    names(dup.l) <- dup.df$var[match(as.numeric(names(dup.l)), dup.df$Cluster_ID)] # name by variable
    dup.l <- numbIf(dup.l) # coerce to numeric if appropriate
    
    # identify data type of each list element by variable name 
    f <- metadata[match(names(dup.l), metadata$ms.vname), "dat.type"]
    
    funs <- funs[f]
    
    # override with variable specific function
    if(!is.null(varSumm)){
      vsf <- which(lapply(varSumm, FUN = class) == "function")
      funs[match(names(varSumm)[vsf], names(dup.l))] <- varSumm[vsf]
    }
    
    # apply the appropriate summarising function according to the data type of each variable
    values <- mapply(function(f, x) f(x), f = funs, x = dup.l)
    names(values) <- cl.ids
    
    # remove all data rows with duplicates (ie all dup.ids) from df and append summarised data rows
    df.w <- df[-which(df$Cluster_ID %in% dup.ids), names(df) %in% c("species","var", "value")]
    add.df <- unique(dup.df[,c("species", "var", "Cluster_ID")])
    add.df <- cbind(add.df, value = values[match(add.df$Cluster_ID, cl.ids)])
    add.df <- add.df[,names(add.df) != "Cluster_ID"]
    df.w <- rbind(df.w, add.df)
    
  }else{
    df.w <- df[, names(df) %in% c("species", "var", "value")]
    }
  wdf <- spread(df.w, key = var, value, convert = F)
  if(add.taxo){
    wdf <- data.frame(species = wdf[,"species"], 
                      master$spp.list[match(wdf$species, master$spp.list$species), 
                                      names(master$spp.list)[names(master$spp.list) %in% ds$taxo.vars]],
                      wdf[, names(wdf) != "species"])
  }
  return(wdf)
}

launch_sr_configurator <- function(sr_configurator, file_setup_path) {
  eval(sr_configurator)
}




check_meta_factors <- function(metadata = sr$metadata) {
  
  if(length(setdiff(metadata$code[!is.na(metadata$scores)], metadata$code[metadata$type %in% c("factor", "Cat", "Bin", "Nom")])) > 0){
    warning("scores entries for traits indicated as non-categorical: /n")
    metadata[metadata$code %in% setdiff(metadata$code[!is.na(metadata$scores)], 
                                        metadata$code[metadata$type %in% c("factor", "Cat", "Bin", "Nom")]),
             c("code", "type", "scores", "levels")]
  }
  
  fact <- metadata[metadata$type %in% c("factor", "Cat", "Bin"),]
  scores <- setNames(strsplit(fact$scores, ";"), fact$code)
  if(any(is.na(scores))){
    stop("scores missing for categorical traits: \n", paste(names(scores[is.na(scores)]), collapse = "\n"))
  }
  levels <- setNames(strsplit(fact$levels, ";"), fact$code)
  if(any(is.na(levels))){
    stop("scores missing for categorical traits: \n", paste(names(levels[is.na(levels)]), collapse = "\n"))
  }
  scores.n <- sapply(scores, FUN = function(x){length(x)})
  levels.n <- sapply(levels, FUN = function(x){length(x)})
  ok <- scores.n == levels.n
  if(any(!ok)){
    print(data.frame( 
      scores.n = scores.n[!ok],
      levels.n = levels.n[!ok]))
    stop("scores.n does not match levels.n for indicated categorical traits")
  }
  print(TRUE)
}
