#' calculate RGB+Nir vegetation indices
#' @description computes several Vegetation Indices based on RGB bands
#' @param rgb a RasterStack or Brick with RGB bands
#' @param indlist comma separated character for desired Vegetation Indices to compute. Select from
#' "NDVI","TDVI","SR","MSR" default=all (see details for further information)
#' @return Returns a raster stack with the selected Vegetation Indices
#' @details
#' ## available indices
#' "NDVI"

#' @note notes if needed
#' @author Andreas Schönberg
#' @references
#' The IDB Project (2020): Index Database (https://www.indexdatabase.de/)
#' @examples

#' @export vegInd_RGB
#' @aliases vegInd_RGB

vegInd_dev<- function(rgb,indlist="all"){

  ### check input
  if(any(indlist=="all")){
    indlist <-c("NDVI","TDVI","SR","MSR")
  }else{indlist=indlist}

  #create notin and check for wrong input
  `%notin%` <- Negate(`%in%`)
  if(any(indlist %notin% c("NDVI","TDVI","SR","MSR"))) {
    stop("wrong Vegetation Index selected or not supported")
  }


  #check if raster is an 3 band layer
  if (raster::nlayers(rgb) < 4)
    stop("Input raster has less than 4 bands")
  red <- rgb[[1]]
  green <- rgb[[2]]
  blue <- rgb[[3]]
  nir <- rgb[[4]]

  #calculate selected indizes
  indices <- lapply(indlist, function(item){
    if (item=="NDVI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (normalized difference vegetation index (NDVI)) ###",sep = "\n")
      NDVI <- (nir-red)/(nir+red)
      names(NDVI) <- "NDVI"
      return(NDVI)

    } else if (item=="TDVI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Transformed Difference Vegetation Index (TDVI)) ###",sep = "\n")
      TDVI<-sqrt(0.5+(nir-red/nir+red))
      names(TDVI) <- "TDVI"
      return(TDVI)

    } else if (item=="SR"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Simple Ratio (SR)) ###",sep = "\n")
      SR<-nir/red
      names(SR) <- "SR"
      return(SR)

    } else if (item=="MSR"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Modified Simple Ratio (MSR)) ###",sep = "\n")
      MSR<-red/((nir/red+1)**0.5)
      names(MSR) <- "MSR"
      return(MSR)

    }

  })# end main loop

  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")

  #output
  return(raster::stack(indices))
} # end function
