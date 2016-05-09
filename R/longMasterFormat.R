#' Convert long dataset to master format.
#' 
#' Rearranges columns of a long formatted dataset to match master data formatting
#' @param data data to be formatted
#' @param master.vars vector containing column names of the master long dataset. 
#' Must be in same order as the master
#' @param data.ID dataset ID code
#' @keywords data 
#' @export
#' @return named list the same length as meta.vars.
#' @examples
#' createMeta()

longMasterFormat <- function(data, master.vars, data.ID){
  
  df <- newMasterData(master.vars, nrow = dim(data)[1])
  
  keep <- names(data)[names(D0) %in% master.vars]
  
  df[match(keep, names(df))] <- data[,keep]
  
  if(any(is.na(df$synonyms))){df$synonyms[is.na(df$synonyms)] <- df$species[is.na(df$synonyms)]}
  if(any(is.na(df$data.status))){df$data.status[is.na(df$data.status)] <- "original"} 
  df$data.ID <- data.ID
  
  return(df)
}
