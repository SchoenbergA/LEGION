#' detect Raster Correlation
#' @description detects Correlation of Rasterlayers in a RasterStack and returns a Rasterstack without the correlating Layers
#' @param Stk a RasterStack
#' @param THvalue numeric in 0.X - Treshold Value for Correlation Value to drop Layers.
#' @param returnCorTab boolean - to return either the cleaned Stack (default=FALSE) or to return the Correlation Table with TRUE


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
#' extpath <-system.file("extdata","lau_mspec.tif",package = "LEGION")
#' mspec <- raster::stack(extpath)
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(exp_rgb,3,2,1)
#' # perform Cor Test
#' y <- detct_RstCor(x,0.7)
#' names(y)
#' # to return the Correlation Matrix
#' z <- detct_RstCor(x,0.7,returnCorTab=TRUE)
#' @export detct_RstCor
#' @aliases detct_RstCor

detct_RstCor <- function(Stk,THvalue,returnCorTab=FALSE){
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
  corrplot::corrplot(cortab)
  lyrnames <-caret::findCorrelation(cortab,abs(THvalue),names=T)
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat(paste("correlating layers detected"))
  # remove selected layers
  cat(" ",sep = "\n")
  cat(paste("dropping from Stack: "))
  cat(paste(lyrnames,collapse=", "),sep="\n")
  cat(" ",sep = "\n")
  cat("### LEGION finished Raster Correlation",sep="\n")
            #output
            if(returnCorTab==FALSE){
            Stk_clean <- dropLayer(Stk,lyrnames)
            return(Stk_clean)}
            if(returnCorTab==TRUE){
            return(cortab)
  }
}# end of function
