#' filter_Rst
#' @description calculates several filters for a single raster
#' @param x parameter

#' @return value and output
#' @details further description
#' @note notes if needed
#' @author Andreas Schönberg
#' @seealso \code{\link{trainControl}},\code{\link{ffs}} ### link to other functions
#' @references # cite articels and papers
#' Meyer, H., Reudenbach, C., Hengl, T., Katurji, M., Nauß, T. (2018): Improving performance of spatio-temporal machine learning models using forward feature selection and target-oriented validation. Environmental Modelling & Software 101: 1-9.
#' @examples # need to lern and test this
#' @export filter_Rst
#' @aliases filter_Rst

filter_Rst <- function(rst,FT_list,FT_sizes){
  #check if raster is an 3 band layer
  if (raster::nlayers(rst) != 1)
    stop("Input is not a single band raster")

  filterStack <- lapply(FT_list, function(item)){
    if (item=="sum"){
    cat(" ",sep = "\n")
    cat("### LEGION calculating (Visible Vegetation Index (VVI)) ###",sep = "\n")
    VVI <- (1 - abs((red - 30) / (red + 30))) *
      (1 - abs((green - 50) / (green + 50))) *
      (1 - abs((blue - 1) / (blue + 1)))
    names(VVI) <- "VVI"
    return(VVI)
    }
}}

# single loop for sizes
x = exp_rst
sizes=c(3,5,7)
test <- function(x,sizes){
  lapply(sizes,function(f){
    spatial <- raster::focal(x,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sum)
    names(spatial) <- paste0("spatial_max_" ,as.factor(f))
    return(spatial)
  })
}

tt <-test(x,sizes)

tt

stk <- stack(tt)
stk
plot(stk)
un <-unlist(tt)
plot(tt[[]])

FT_sizes <- c(3,5,7)

for(f in 1:length(FT_sizes)){
  spatial <- raster::focal(x,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sum)
  return(spatial)
}
spatial
filter_Rst(exp_rgb)
data(exp_rgb)
data(exp_rst)
x<-exp_rst
exp_rgb
x
f=3
test <- raster::focal(x,w=matrix(1/(f*f),nrow=f,ncol=f))
plot(test)
test
test2 <- raster::focal(x,w=matrix(1/(f*f),nrow=f,ncol=f),fun=mean)
plot(test2)
test3 <- raster::focal(x,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sum)
plot(test3)
test <- raster::focal(x,w=matrix(1/(f*f),nrow=f,ncol=f),fun=modal)
