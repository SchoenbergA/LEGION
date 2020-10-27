#' Calculate RGB indices
#' @description computes several indices based on RGB bands
#' @param rgb a RasterStack with RGB bands
#' @param red the band/layer number of band 'red'
#' @param green the band/layer number of band 'green'
#' @param blue the band/layer number of band 'blue'
#' @param indlist comma-separated character combinations of the desired indices. Select from
#' "VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI", default=all. See details
#' @return returns a RasterStack with the selected indices
#' @details
#' ## available indices
#' * "VVI" Visible Vegetation Index (1 - abs((red - 30) / (red + 30))) * (1 - abs((green - 50) / (green + 50))) *(1 - abs((blue - 1) / (blue + 1)))
#' * "VARI" Visible Atmospherically Resistant Index (green-red)/(green+red-blue)
#' * "NDTI" Normalized Difference Turbidity Index (red-green)/(red+green)
#' * "RI" Redness index (red**2/(blue*green**3)
#' * "CI" Soil Colour Index (red-green)/(red+green)
#' * "BI" Brightness Index sqrt((red**2+green**2+blue*2)/3)
#' * "SI" Spectra Slope Saturation Index (red-blue)/(red+blue)
#' * "HI" Primary Colours Hue Index (2*red-green-blue)/(green-blue)
#' * "TGI" Triangular Greenness Index (-0.5*(190*(red - green)- 120*(red - blue))
#' * "GLI" Green Leaf Index (2*green-red-blue)/(2*green+red+blue)
#' * "NGRDI" Normalized Green Red Difference Index (green-red)/(green+red)
#' @author Andreas Schönberg
#' @references
#' The IDB Project (2020): Index Database (https://www.indexdatabase.de/)
#' @examples
#' ### load data
#' require(raster)
#' require(LEGION)
#' mspec <- raster::stack(system.file("extdata","lau_mspec.tif",package = "LEGION"))
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(mspec,3,2,1)
#' plot(x)
#' ### select specific vegetation indices
#' vi <-c("VVI","SI","GLI")
#' y <-LEGION::vegInd_RGB(mspec,3,2,1,indlist=vi)
#' plot(y)
#' @export vegInd_RGB
#' @aliases vegInd_RGB

vegInd_RGB<- function(rgb,red=NULL,green=NULL,blue=NULL,indlist="all"){

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
  if (any(is.null(red),is.null(green),is.null(blue))==TRUE){
    stop("no bands or less bands defined")
  }
  red <- rgb[[red]]
  green <- rgb[[green]]
  blue <- rgb[[blue]]

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
