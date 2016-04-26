

# dtype.over: named vector to override default and assign summarising function for whole data types.
# var.over: named vector to override default and assign summarising function for individual variables. 
  # Can also take the name of a prefered dataset to use

widenMaster <- function(vars, species, master, metadata, datSumm = NULL, varSumm = NULL, dupONLY = T){

  require(dplyr)
  require(tidyr)
  
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  numbIf <- function(X){lapply(X, FUN = function(x) if(is.numeric(t <- type.convert(x))) t else x)}
  

  df <- master[master$species %in% species & master$var %in% vars, ]
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
      df.w <- df[-which(df$Cluster_ID %in% dup.ids), names(df) %in% c("species","order", "family", "var", "value")]
      add.df <- unique(dup.df[,c("species","order", "family","var", "Cluster_ID")])
      add.df <- cbind(add.df, value = values[match(add.df$Cluster_ID, cl.ids)])
      add.df <- add.df[,names(add.df) != "Cluster_ID"]
      df.w <- rbind(df.w, add.df)

  }else{df.w <- df[, names(df) %in% c("species", "order", "family", "var", "value")]}

 
  
  wdf <- spread(df.w, key = var, value, convert = F)
  
  return(wdf)
  
}

