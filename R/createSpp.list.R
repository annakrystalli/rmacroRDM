#' Create spp.list 
#' 
#' create spp.list and add taxonomic data
#' @param species a vector of species names. Blanks between species names not allowed, use "_" instead
#' @param taxo.dat a dataframe containing species taxonomic information. Must include a species column.
#' @param taxo.vars a vector of taxo.dat column names to be included in the spp.list. if "all" then all data contained in taxo.dat are included.
#' @keywords taxonomy 
#' @export
#' @return spp.list dataframe .
#' @examples
#' createSpp.list()

createSpp.list <- function(species, taxo.dat = NULL, taxo.vars = NULL){
  
  if(is.null(taxo.vars)){
    spp.list <- data.frame(species = species, master.spp = T, rel.spp = NA, 
                           taxo.status = "original")
    return(spp.list)}
  
  if(any(!species %in% taxo.dat$species)){
    print(species[!species %in% taxo.dat$species])
    stop("species data missing in taxo.dat")}
  
  if(taxo.vars == "all"){taxo.dat <- taxo.dat[taxo.dat$species == species, 
                                              names(taxo.dat != "species")]}else{
                                                if(any(!taxo.vars %in% names(taxo.dat))){
                                                  print(taxo.vars[!taxo.vars %in% names(taxo.dat)])
                                                  stop("taxo.vars data missing in taxo.dat")}else{
                                                    taxo.dat <- taxo.dat[taxo.dat$species == species, taxo.vars]}}
  
  spp.list <- data.frame(species = species, master.spp = T, rel.spp = NA, 
                         taxo.status = "original", taxo.dat)
}

