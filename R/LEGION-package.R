#' LEGION Artfically Layers
#'
#' @name LEGION - package
#' @docType package
#' @title LEGION - Artifically layers - a tool for computing several AL
#' @description In general the \code{LEGION} package provides tool to compute a huge stack of artifically layers
#' to use in maschine learing or ordination/cluster analysis. Those methods require a lot of informations, the more the better (unless overfitting).
#' With \code{LEGION} the user can first compute several indices and filters to generate artifically layers
#' from just a RGB or multispectral image to use in maschine learing or ordination/cluster analysis.
#' Further \code{LEGION} provides a function to detect higly correlating layers (which could lead to overfitting or just high time consumption in processing).
#'
#' @note In general a 'brute force' usage of \code{LEGION} is not recommended. EG to compute all available filters with sizes of 1 to 101 on all availbe indices would lead to hundreds artifally layers.
#' This could take hours to days to compute (especially for large rasters) which makes little sense.
#'
#'  But  \code{LEGION} helps to reduce "overthinking" in planning which ALs should be used for a maschine learing or ordination/cluster analysis task.
#'  A recommended way to generate a Stack with much information but less correlation in relatifly short time is to first generate the indices. Than to clean the Stack from correlating ALs and THAN to filter the cleaned Stack.
#'
#' @author Andreas Schönberg
#' @import raster
#' @keywords package
NULL
#' @docType data
#' @name lau_mspec - data
#' @title A multispectral image (RGB+nir) with some trees in the lautaret vally in the france alps.
#' @description An RGB orthophoto merged with the Nir band of a IRC image. Bands sequence is: 1-blue,2-green,3-red,4-nir. Resolution 0.15 meter.
#' @format \code{"raster::raster"}
NULL