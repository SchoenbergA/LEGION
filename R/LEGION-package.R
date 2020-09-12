#' LEGION
#'
#' @name LEGION - package
#' @docType package
#' @title LEGION - a tool for computing several artificial layers (AL)
#' @description In general the \code{LEGION} package provides tools to compute a huge stack of artifical layers
#' to use in maschine learing or ordination/cluster analysis. These methods require lots of information, the more the better (unless it leads to overfitting).
#' With \code{LEGION} the user can first compute several indices and apply filter to generate artifical layers of a RasterLayer or a RasterStack.
#' Further \code{LEGION} provides a function to detect higly correlating layers (which could lead to overfitting and/or high computing time while processing).
#'
#' @note In general a 'brute force' use of \code{LEGION} is not recommended. E.g. applying all available filters with multiple sizes of MovingWindows on all availabe indices would lead to multiple hundreds of artifal layers.
#' This could take hours to days to compute (especially in the case of large RasterStacks) which makes little sense.
#'
#'  On the other hand  \code{LEGION} helps to reduce "overthinking" in planning which ALs should be used for a maschine learing or ordination/cluster analysis task.
#'  A recommended way to generate a RasterStack with the required information but minimal correlation in a relatively short time is to first generate the indices and then to clean the RasterStack from correlating ALs and then successively to filter the cleaned RasterStack.
#'
#' @author Andreas Schönberg
#' @import raster
#' @keywords package
NULL
#' @docType data
#' @name lau_mspec - data
#' @title A multispectral image (RGB+NIR) with some trees in the Lautaret-valley in the French Alps.
#' @description An RGB orthophoto merged with the NIR band of a IRC image. The bandsequence is: 1-blue, 2-green, 3-red, 4-nir, resolution 0.15 meter.
#' @format \code{"raster::raster"}
NULL
