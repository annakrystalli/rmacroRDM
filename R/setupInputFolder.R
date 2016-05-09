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
