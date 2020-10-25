#' Detect Raster Homogeneity v2
#' @description detects homogeneity of Rasterlayers in a RasterStack and drops RasterLayers with homogeneity higher than a set Threshold Value.
#' @param Stk a RasterStack
#' @param THvalue numeric  - in percent (0.x) Threshold Value for homogeneity Value to drop Layers.
#' @param valueRange numeric - in percent (0.x) Range of Values with most data (see details).
#' @return Returns the RasterStack without homogeneous RasterLayers.
#' @details
#' This function is used to test a RasterStack for homogeneous RasterLayers.A RasterLayer is makred as homogenious if >= x% of the data is distributed in y % of the value range.
#' E.G. If 90% (THvalue=0.9) of the Raster cells have values within 10% of the value range (valueRange=0.1) the RasterLayer is dropped if due to homogeneity.
#' @note
#' * To perform the test for homogeneity the data is cleand from INF and NA values. Further the data will be normalized to set 100 breaks representing 1% of the data range.
#'
#' * The RasterLayers in the output Stack will be selected by their homogeneity and are NOT manipulated (not clean from INF or NA or normalized)
#' @author Andreas Schönberg
#' @examples
#' ### load data
#' extpath <-system.file("extdata","lau_mspec.tif",package = "LEGION")
#' mspec <- raster::stack(extpath)
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indices
#' x <-LEGION::vegInd_RGB(mspec,3,2,1)
#' plot(x)
#' ### homogenity if 90% of data represent 10% of the data range
#' hmgy90 <-detct_RstHmgy (x,THvalue=0.9,valueRange=0.1)
#' hmgy90
#' ### homogenity if 70% of data represent 10% of the data range
#' hmgy90 <-detct_RstHmgy (x,THvalue=0.9,valueRange=0.1)
#' ### homogenity if 70% of data represent 5% of the data range
#' hmgy90 <-detct_RstHmgy (x,THvalue=0.9,valueRange=0.1)

#' @export detct_RstHmgy2
#' @aliases detct_RstHmgy2


detct_RstHmgy2 <- function(Stk,valueRange,THvalue){
  cat("### LEGION testing Raster homogeneity",sep="\n")

  # check all Rst for homogeneity
  hmgy <-lapply(1:nlayers(Stk), function(i){


    # seperate single raster and get values
    rst <- Stk[[i]]
    nc <- ncell(rst)
    val <- getValues(rst)
    # handle inf
    val[!is.finite(val)] <- NA
    # handle NA
    val <- val[!is.na(val)]
    # normlaize
    nmlz <- function(x){(x-min(x))/(max(x)-min(x))}
    val_n <- nmlz(val)
    # compute histogramm, breaks @ 1% of data

    h <- hist(val_n,plot=F, breaks= seq(0,1,0.01))

    # sort counts decreasing and get count in % of total cells
    sh <-sort(h$counts,decreasing = T)/nc

    vr <-round(sum(sh[1:(valueRange*100)]),digits = 4)
    check <-vr>=THvalue

    # compare amount of data in x % with THvalue

    if(check==TRUE){
      cat(" ",sep = "\n")
      cat(names(Stk[[i]]),sep = "\n")
      cat(paste("Layer has: ",round(vr*100,digits = 4),"% of values in",valueRange*100,"% of the value range ---> drop"))
      cat(" ",sep = "\n")
      drop <- names(Stk[[i]])


    } else {
      cat(" ",sep = "\n")
      cat(names(Stk[[i]]),sep = "\n")
      cat(paste("Layer has: ",round(vr*100,digits = 4),"% of values in",valueRange*100,"% of the value range ---> keep"))
      cat(" ",sep = "\n")
      drop <- "NA"
    }

  }# end lapply function

  )# end lapply


  # unlist and drop layers
  hmgy <-unlist(hmgy)
  hmgy = hmgy[hmgy!= "NA"]
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  # fork for any homogeneity, no homogeneity or full homogeneity
  if(length(hmgy)<nlayers(Stk) & length(hmgy)!=0){
    cat(paste("layers with homogeneity detected"))
    # remove selected layers
    Stk_clean <- dropLayer(Stk,hmgy)
    cat(" ",sep = "\n")
    cat(paste("dropping from Stack: "))
    cat(paste(hmgy,collapse=", "),sep="\n")
    cat(" ",sep = "\n")
    cat("### LEGION finished Raster homogeneity detection",sep="\n")
    return(Stk_clean)
    # if no layers with homogeneity are detected
  } else if (length(hmgy)==nlayers(Stk)){
    cat("homogeneity for all RasterLayers in the RasterStack detected, returning empty Stack")
    Stk_clean <- dropLayer(Stk,hmgy)
    return(Stk_clean)
    # no homogeneity
  } else {
    cat("no layers with homogeneity detected, returning the full Stk")
    return(Stk)
  }# end fork for homogeneity

}# end of function
