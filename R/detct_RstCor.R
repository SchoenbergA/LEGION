#' detect Raster Correlation
#' @description detects Correlation of Rasterlayers in a RasterStack and returns a Rasterstack without the correlating Layers
#' @param Stk a RasterStack
#' @param THvalue numeric in 0.X - Treshold Value for Correlation Value to drop Layers.


#' @return Returns a raster stack with the layers correlation less than the Treshold Value.
#' @details
#' This function is used to test a RasterStack for correlating Layers. All layers which have a greater Correlation than the THvalue
#' will be dropped from the Stack. E.G. if THvalue=0.9 all Layers with corvalues of >0.9 and <-0.9 will be dropped.
#' @note
#' * To perform a correlation Test all Values are cleaned from INF and or NA Values.
#' * The output Stack is NOT cleaned from INF or NA Values.
#' @author Andreas Schönberg
#' @examples
#' ### load data to compute RGB Indices
#' exp_rgb <-LEGION::exp_rgb
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(exp_rgb)
#' # perform Cor Test
#' y <- detct_RstCor(x,0.7)
#' names(y)
#' @export detct_RstCor
#' @aliases detct_RstCor

detct_RstCor <- function(Stk,THvalue){
  cat("### LEGION testing Raster Correlation",sep="\n")
  #get values from Stack
  val <- getValues(Stk)
  if(any(is.infinite(val))==TRUE){
    nINF <- sum(is.infinite(val))
    # set inf to NA
    cat(" ",sep = "\n")
    cat(paste("INF values detected: setting ",nINF,"INF to NA"))
    val[mapply(is.infinite, val)] <- NA
  }

  if(any(is.na(val))==TRUE){
    nNA <- sum(is.na(val))
    # delete NAs
    cat(" ",sep = "\n")
    cat("NAs detected: deleting",nNA," NAs")
    val <-na.omit(val)
  }

  #cortest and select correlating layers
  cortab <- cor(val)
  lyrnames <-caret::findCorrelation(cortab,abs(THvalue),names=T)
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat(paste("correlating layers detected"))
  # remove selected layers
  cat(" ",sep = "\n")
  cat(paste("dropping from Stack:"))
  cat(paste(lyrnames,collapse=", "),sep="\n")
  cat(" ",sep = "\n")
  cat("### LEGION finished Raster Correlation",sep="\n")
  #output
  Stk_clean <- dropLayer(Stk,lyrnames)
  return(Stk_clean)
}# end of function
