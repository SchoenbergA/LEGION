#' calculate RGB+Nir vegetation indices
#' @description computes several Vegetation Indices based on RGB bands
#' @param rgb a RasterStack or Brick with RGB bands
#' @param indlist comma separated character for desired Vegetation Indices to compute. Select from
#' "NDVI", default=all (see details for further information)
#' @return Returns a raster stack with the selected Vegetation Indices
#' @details
#' ## available indices
#' "NDVI"

#' @note notes if needed
#' @author Andreas Schönberg
#' @references
#' The IDB Project (2020): Index Database (https://www.indexdatabase.de/)
#' @examples
#' ### load data
#' data(exp_rgb)
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(exp_rgb)
#' plot(x)
#' ### select specific vegetation indices
#' vi <-c("VVI","SI","GLI")
#' y <-LEGION::vegInd(exp_rgb,indlist=vi)
#' plot(y)
#' @export vegInd_RGB
#' @aliases vegInd_RGB

vegInd_dev<- function(rgb,indlist="all"){

  ### check input
  if(any(indlist=="all")){
    indlist <-c("NDVI","VARI","VVI")
  }else{indlist=indlist}

  #create notin and check for wrong input
  `%notin%` <- Negate(`%in%`)
  if(any(indlist %notin% c("NDVI","VARI","VVI"))) {
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

    } else if (item=="VARI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Visible Atmospherically Resistant Index (VARI)) ###",sep = "\n")
      VARI<-(green-red)/(green+red-blue)
      names(VARI) <- "VARI"
      return(VARI)

    }else if (item=="VVI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Visible Vegetation Index (VVI)) ###",sep = "\n")
      VVI <- (1 - abs((red - 30) / (red + 30))) *
        (1 - abs((green - 50) / (green + 50))) *
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)
    }
  })# end main loop

  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")

  #output
  return(raster::stack(indices))
} # end function

