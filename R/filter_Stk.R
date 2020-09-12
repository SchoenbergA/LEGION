#' Wrapper for 'filter_Rst' to filter RasterStacks
#' @description applies several filter functions to each RasterLayer in a RasterStack.
#' @param Stk a RasterStack.
#' @param fLS comma-separated character combinations of the desired filter functions. Select from
#' "sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert".
#' default = all; (see details).
#' @param sizes numeric - a single value or combinations for the MovingWindow, number must be odd.
#' @param layernames optional - comma-seperated character combinations of the desired layernames.
#' @return returns a RasterStack with the desired filtered artificial layers.
#' @details
#' ## available filter functions
#' * "sum" - sum of all cells in a MovingWindow
#' * "min" - minimum value of all cells in a MovingWindow
#' * "max" - maximum value of all cells in a MovingWindow
#' * "mean"- mean value of all cells in a MovingWindow
#' * "sd"  - standard deviation of all cells in a MovingWindow
#' * "modal" - most frequent value of all cells in a MovingWindow
#' * "sobel" - sobel edge detection filter in horizontal and vertical directions
#' * "sobel_hrzt" - sobel edge detection filter in horizontal direction only
#' * "sobel_vert" - sobel edge detection filter in vertical direction only
#' @author Andreas Schönberg
#' @seealso \code{\link{focal}},\code{\link{filter_Rst}}
#' @examples
#' ### load data
#' extpath <-system.file("extdata","lau_mspec.tif",package = "LEGION")
#' mspec <- raster::stack(extpath)
#' ### compute all filters or every layer in stack
#' x <- filter_Stk(mspec,sizes=3)
#' x # note that the names are basic set x.1
#' ### define layernames
#' ln <- c("blue","green","red","nir")
#' z <- filter_Stk(mspec,fLS="sum",sizes=3,layernames=ln)
#' names(z)
#' ### compute specific filters
#' flist <- c("modal","sobel_vert","mean")
#' y <- filter_Stk(mspec,fLS=flist,sizes=c(3,5,7))
#' y
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


