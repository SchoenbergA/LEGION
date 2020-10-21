#' detect Raster homogeneity
#' @description detects homogeneity of Rasterlayers in a RasterStack and drops RasterLayers with homogeneity higher than a Treshold Value.
#' @param Stk a RasterStack
#' @param THvalue numeric  - in percent (0.x) Treshold Value for homogeneity Value to drop Layers.
#' @return Returns the RasterStack without homogenious RasterLayers.
#' @details
#' This function is used to test a RasterStack for homogenious RasterLayers. All RasterLayers with any values which represent more or equal percentage (THvalue) of all cells are dropped.
#' E.g. if a RasterLayer with ncell= 100 contains 95 cell with value 1 it will be dropped if the THvalue is 0.95 or less.
#' @note
#' * The output Stack is NOT cleaned from INF or NA Values.
#' @author Andreas Schönberg
#' @examples
#' ### load data
#' extpath <-system.file("extdata","lau_mspec.tif",package = "LEGION")
#' mspec <- raster::stack(extpath)
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(mspec,3,2,1)
#' plot(x)
#' ### test for homogeneity with 90% occurance of any cell value
#' hmgy90 <-detct_RstHmgy (x,0.9)
#' hmgy90
#' ### test for homogeneity with 10% occurance of any cell value
#' hmgy10 <-detct_RstHmgy (x,0.1)
#' hmgy10 # returns emtpy RasterStack
#' ### test for homogeneity with 100% occurance of any cell value
#' hmgy90 <-detct_RstHmgy (x,1.0)
#' hmgy90 # returns full RasterStack

#' @export detct_RstHmgy
#' @aliases detct_RstHmgy

detct_RstHmgy <- function(Stk,THvalue){
  cat("### LEGION testing Raster homogeneity",sep="\n")



    # check all Rst for homogeneity
    hmgy <-lapply(1:nlayers(Stk), function(i){
      histo <- hist(Stk[[i]],plot=FALSE)
      Ncell <- ncell(Stk[[i]])
      THcell<- Ncell*THvalue
      if(any(histo$counts>=THcell)==TRUE){
        drop <- names(Stk[[i]])
      } else {
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
                cat("homogeneity for all RasterLayers in the RasterStack detected, returning empty Stk")
                Stk_clean <- dropLayer(Stk,hmgy)
                return(Stk_clean)
                       # no homogeneity
                       } else {
                       cat("no layers with homogeneity detected, returning the full Stk")
                       return(Stk)
                       }# end fork for homogeneity

}# end of function
