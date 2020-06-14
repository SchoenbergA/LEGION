#' filter_Stk
#' @description calculates several filters for each Single Raster in a RasterStack
#' @param Stk a Raster Stack
#' @param fLS comma separated character for desired filter functions. Select from
#' "sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert","sobel"
#' default = all (see details for further informations)
#' @param sizes numeric values for the moving window, must be odd
#' @param layernames optional - comma seperated character for desired layernames
#' @return Returns a raster stack with the selected filters
#' @details further description
#' @note notes if needed
#' @author Andreas Schönberg
#' @seealso \code{\link{focal}},\code{\link{filter_Rst}}
#' @examples
#' ### load data
#' data(exp_rgb)
#' ### compute all filter
#' x <- filter_Stk(exp_rgb,sizes=3)
#' x
#' ### compute specific filters
#' flist <- c("modal","sobel_vert","mean")
#' y <- filter_Stk(exp_rst,fLS=flist,sizes=c(3,5,7))
#' y
#' ### define layernames
#' ln <-c("red","green","blue")
#' z <- filter_Stk(exp_rgb,fLS="sum",sizes=3,layernames=ln)
#' names(z)
#' @export filter_Stk
#' @aliases filter_Stk

filter_Stk <- function(stk,fLS="all",sizes,layernames=names(stk)){

  #check name input
  if(length(layernames)!=nlayers(stk)){
    stop("incorrect number of names in 'layername'")
  }
  names(stk) <- layernames


  all <-lapply(1:nlayers(stk), function(i){
    cat(" ",sep = "\n")
    cat(paste0("starting filter for layer ",names(stk[[i]])))
    cat(" ",sep = "\n")
    rt <-filter_Rst(stk[[i]],fLS,sizes)
    cat(paste0("### layer ",as.factor(i)," / ",nlayers(stk)))
    cat(" ",sep = "\n")
  return(rt)
})
  #handle output format
  unLS <- unlist(all)
  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")
  return(raster::stack(unLS))


}# end function


