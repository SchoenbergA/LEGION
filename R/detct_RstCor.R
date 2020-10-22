#' Detect Raster Correlation
#' @description detects correlation of RasterLayers in a RasterStack
#' @param Stk a RasterStack
#' @param THvalue numeric as 0.X - threshold value for correlation value of correlating layers to drop correlating layers (see details).
#' @param returnCorTab boolean - to return either the cleaned RasterStack (FALSE) or to return the correlation table (TRUE); default= FALSE


#' @return returns a RasterStack with the layers correlating less than the treshold value.
#' @details
#' This function is used to test a RasterStack on the correlation of the RasterLayers. All RasterLayers which have a higher correlation value than 'THvalue'
#' will be dropped from the RasterStack. E.g. if THvalue=0.9 all RasterLayers with correlation values >0.9 and  < -0.9 will be dropped.
#' @note
#' * To perform a correlation test all values are cleaned of INF and/or NA values.
#' * The output RasterStack is NOT cleaned from INF and/or NA values.
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
  cat(paste("dropping from Stack:"))
  cat(paste(lyrnames,collapse=", "),sep="\n")
  cat(" ",sep = "\n")
  cat("### LEGION finished testing Raster Correlation",sep="\n")
            #output
            if(returnCorTab==FALSE){
            Stk_clean <- dropLayer(Stk,lyrnames)
            return(Stk_clean)}
            if(returnCorTab==TRUE){
            return(cortab)
  }
}# end of function
