#' filter_Rst
#' @description calculates several filters for a single raster
#' @param rst a single band raster layer
#' @param fLS comma separated character for desired filter functions. Select from
#' "sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert","sobel"
#' default = all (see details for further informations)
#' @param sizes numeric values for the moving window, must be odd

#' @return Returns a raster stack with the selected filters
#' @details further description
#' @note notes if needed
#' @author Andreas Schönberg
#' @seealso \code{\link{trainControl}},\code{\link{ffs}} ### link to other functions
#' @references
#' Meyer, H., Reudenbach, C., Hengl, T., Katurji, M., Nauß, T. (2018): Improving performance of spatio-temporal machine learning models using forward feature selection and target-oriented validation. Environmental Modelling & Software 101: 1-9.
#' @examples # need to lern and test this
#' @export filter_Rst
#' @aliases filter_Rst

filter_Rst <- function(rst,fLS,sizes){
  filterstk <-lapply(fLS, function(item){

    # sum filter
    if (item=="sum"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sum filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sum  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        sumLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sum)
        names(sumLS) <- paste0(names(rst),"_sum" ,as.factor(f))
        stack(sumLS)
        return(sumLS)
      })
    }#end

    # min filter
    else if (item=="min"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating minimum filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting min  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        minLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=min)
        names(minLS) <- paste0(names(rst),"_min" ,as.factor(f))
        return(minLS)
      })
    }#end

    # max filter
    else if (item=="max"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating maximum filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting max  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        maxLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=max)
        names(maxLS) <- paste0(names(rst),"_max" ,as.factor(f))
        return(maxLS)
      })
    }#end

    # sd filter
    else if (item=="sd"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating standart deviation filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sd   ",as.factor(f),"*",as.factor(f),sep = "\n"))
        sdLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sd)
        names(sdLS) <- paste0(names(rst),"_sd" ,as.factor(f))
        return(sdLS)
      })
    }#end

    # mean filter
    else if (item=="mean"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating mean filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting mean  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        meanLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=mean)
        names(meanLS) <- paste0(names(rst),"_mean" ,as.factor(f))
        return(meanLS)
      })
    }#end

    # modal filter
    else if (item=="modal"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating modal filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting modal  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        modalLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=modal)
        names(modalLS) <- paste0(names(rst),"_modal" ,as.factor(f))
        return(modalLS)
      })
    }#end

    else if (item=="sobel"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sobel filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sobel  ",as.factor(f),"*",as.factor(f),sep = "\n"))
      range = f/2
      mx = matrix(nrow = f, ncol = f)
      my = mx

      for(i in seq(-floor(range), floor(range))){
        for(j in seq(-floor(range), floor(range))){
          mx[i+ceiling(range),j+ceiling(range)] = i / (i*i + j*j)
          my[i+ceiling(range),j+ceiling(range)] = j / (i*i + j*j)
        }
      }

      mx[is.na(mx)] = 0
      my[is.na(my)] = 0

      sobelLS <- sqrt(raster::focal(rst,mx,fun=sum)**2+
                      raster::focal(rst,my,fun=sum)**2 )
      names(sobelLS) <- paste0(names(rst),"_sobel" ,as.factor(f))
      return(sobelLS)
      })
    }#end

    else if (item=="sobel_hrzt"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sobel horizontale filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sobel horizontale  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        range = f/2
        mx = matrix(nrow = f, ncol = f)
        my = mx

        for(i in seq(-floor(range), floor(range))){
          for(j in seq(-floor(range), floor(range))){
            mx[i+ceiling(range),j+ceiling(range)] = i / (i*i + j*j)
            my[i+ceiling(range),j+ceiling(range)] = j / (i*i + j*j)
          }
        }

        mx[is.na(mx)] = 0
        my[is.na(my)] = 0

        sobel_hLS <- raster::focal(x, mx, fun = sum)
        names(sobel_hLS) <- paste0(names(rst),"_sobel_h" ,as.factor(f))
        return(sobel_hLS)
      })
    }#end

    else if (item=="sobel_vert"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sobel vertical filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sobel vertical  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        range = f/2
        mx = matrix(nrow = f, ncol = f)
        my = mx

        for(i in seq(-floor(range), floor(range))){
          for(j in seq(-floor(range), floor(range))){
            mx[i+ceiling(range),j+ceiling(range)] = i / (i*i + j*j)
            my[i+ceiling(range),j+ceiling(range)] = j / (i*i + j*j)
          }
        }

        mx[is.na(mx)] = 0
        my[is.na(my)] = 0

        sobel_vLS <- raster::focal(x, mx, fun = sum)
        names(sobel_vLS) <- paste0(names(rst),"_sobel_v" ,as.factor(f))
        return(sobel_vLS)
      })
    }#end

  })#end main lapply

  #########################################

  #handle output format
  unLS <- unlist(filterstk)
  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")
  return(raster::stack(unLS))

} # end fun


###
###



