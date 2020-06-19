#' calculate RGB vegetation indices
#' @description computes several Vegetation Indices based on RGB bands
#' @param rgb a RasterStack or Brick with RGB bands
#' @param indlist comma separated character for desired Vegetation Indices to compute. Select from
#' "VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI", default=all (see details for further information)
#' @return Returns a raster stack with the selected Vegetation Indices
#' @details
#' ## available indices
#' "VVI"
#' "VARI"
#' "NDTI"
#' "RI"
#' "CI"
#' "BI"
#' "SI"
#' "HI"
#' "TGI"
#' "GLI"
#' "NGRDI"
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

vegInd_RGB<- function(rgb,indlist="all"){

  ### check input
  if(any(indlist=="all")){
    indlist <-c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI")
  }else{indlist=indlist}

  #create notin and check for wrong input
  `%notin%` <- Negate(`%in%`)
  if(any(indlist %notin% c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI"))) {
    stop("wrong Vegetation Index selected or not supported")
  }


  #check if raster is an 3 band layer
  if (raster::nlayers(rgb) != 3)
    stop("Input raster has more or less than 3 bands")
  red <- rgb[[1]]
  green <- rgb[[2]]
  blue <- rgb[[3]]

  #calculate selected indizes
  indices <- lapply(indlist, function(item){
    if (item=="VVI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Visible Vegetation Index (VVI)) ###",sep = "\n")
      VVI <- (1 - abs((red - 30) / (red + 30))) *
        (1 - abs((green - 50) / (green + 50))) *
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)

    } else if (item=="VARI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Visible Atmospherically Resistant Index (VARI)) ###",sep = "\n")
      VARI<-(green-red)/(green+red-blue)
      names(VARI) <- "VARI"
      return(VARI)

    } else if (item=="NDTI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Normalized difference turbidity index (NDTI)) ###",sep = "\n")
      NDTI<-(red-green)/(red+green)
      names(NDTI) <- "NDTI"
      return(NDTI)

    } else if (item=="RI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Redness index (RI)) ###",sep = "\n")
      RI<-red**2/(blue*green**3)
      names(RI) <- "RI"
      return(RI)

    } else if (item=="CI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Soil Colour Index (CI)) ###",sep = "\n")
      CI<-(red-green)/(red+green)
      names(CI) <- "CI"
      return(CI)

    } else if (item=="BI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Brightness Index (BI)) ###",sep = "\n")
      BI<-sqrt((red**2+green**2+blue*2)/3)
      names(BI) <- "BI"
      return(BI)

    } else if (item=="SI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Spectra Slope Saturation Index (SI)) ###",sep = "\n")
      SI<-(red-blue)/(red+blue)
      names(SI) <- "SI"
      return(SI)

    } else if (item=="HI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Primary colours Hue Index (HI)) ###",sep = "\n")
      HI<-(2*red-green-blue)/(green-blue)
      names(HI) <- "HI"
      return(HI)

    } else if (item=="TGI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Triangular greenness index (TGI)) ###",sep = "\n")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      names(TGI) <- "TGI"
      return(TGI)

    } else if (item=="GLI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Green leaf index (GLI)) ###",sep = "\n")
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      names(GLI) <- "GLI"
      return(GLI)

    } else if (item=="NGRDI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Normalized green red difference index  (NGRDI)) ###",sep = "\n")
      NGRDI<-(green-red)/(green+red)
      names(NGRDI) <- "NGRDI"
      return(NGRDI)

    }
  })
  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")

  return(raster::stack(indices))
}
