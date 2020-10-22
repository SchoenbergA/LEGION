#' Calculate RGB+Nir indices
#' @description computes several indices based on RGB+NIR bands
#' @param mspec a RasterStack with RGB+NIR bands
#' @param red the band/layer number of band 'red'
#' @param green the band/layer number of band 'green'
#' @param blue the band/layer number of band 'blue'
#' @param nir the band/layer number of band 'Near Infrared (NIR)'
#' @param indlist comma-separated character combinations of the desired indices. Select from
#' "NDVI","TDVI","SR","MSR" default=all. See details
#' @return Returns a RasterStack with the selected indices
#' @details
#' ## available indices
#' * "NDVI" - Normalized Difference Vegetation Index; (nir-red)/(nir+red)
#' * "TDVI" - Transformed Difference Vegetation Index; sqrt(0.5+(nir-red/nir+red))
#' * "SR" - Simple Ratio; red/nir
#' * "MSR" - Modified Simple Ratio; red/((nir/red+1)**0.5)
#'
#' @note notes if needed
#' @author Andreas Schönberg
#' @references
#' The IDB Project (2020): Index Database (https://www.indexdatabase.de/)
#' @examples
#' ### load data
#' extpath <-system.file("extdata","lau_mspec.tif",package = "LEGION")
#' mspec <- raster::stack(extpath)
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_mspec(mspec,3,2,1,4)
#' plot(x)
#' ### select specific vegetation indices
#' vi <-c("NDVI","TDVI")
#' y <-LEGION::vegInd_mspec(mspec,3,2,1,4,indlist=vi)
#' plot(y)
#' @export vegInd_mspec
#' @aliases vegInd_mspec

vegInd_mspec <- function(mspec,red=NULL,green=NULL,blue=NULL,nir=NULL,indlist="all"){

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
  if (any(is.null(red),is.null(green),is.null(blue),is.null(nir))==TRUE){
    stop("no bands or less bands defined")
  }
  red <- mspec[[red]]
  green <- mspec[[green]]
  blue <- mspec[[blue]]
  nir <- mspec[[nir]]

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
      SR<-red/nir
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
