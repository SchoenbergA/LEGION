% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegInd_Nir.R
\name{vegInd_mspec}
\alias{vegInd_mspec}
\title{Calculate RGB+Nir indices}
\usage{
vegInd_mspec(
  mspec,
  red = NULL,
  green = NULL,
  blue = NULL,
  nir = NULL,
  indlist = "all"
)
}
\arguments{
\item{mspec}{a RasterStack with RGB+NIR bands}

\item{red}{the band/layer number of band 'red'}

\item{green}{the band/layer number of band 'green'}

\item{blue}{the band/layer number of band 'blue'}

\item{nir}{the band/layer number of band 'Near Infrared (NIR)'}

\item{indlist}{comma-separated character combinations of the desired indices. Select from
"NDVI","TDVI","SR","MSR" default=all. See details}
}
\value{
Returns a RasterStack with the selected indices
}
\description{
computes several indices based on RGB+NIR bands
}
\details{
\subsection{available indices}{
\itemize{
\item "NDVI" - Normalized Difference Vegetation Index; (nir-red)/(nir+red)
\item "TDVI" - Transformed Difference Vegetation Index; sqrt(0.5+(nir-red/nir+red))
\item "SR" - Simple Ratio; red/nir
\item "MSR" - Modified Simple Ratio; red/((nir/red+1)**0.5)
}
}
}
\note{
notes if needed
}
\examples{
### load data
extpath <-system.file("extdata","lau_mspec.tif",package = "LEGION")
mspec <- raster::stack(extpath)
names(mspec)<- c("blue","green","red","nir")
### compute all vegetation indizes
x <-LEGION::vegInd_mspec(mspec,3,2,1,4)
plot(x)
### select specific vegetation indices
vi <-c("NDVI","TDVI")
y <-LEGION::vegInd_mspec(mspec,3,2,1,4,indlist=vi)
plot(y)
}
\references{
The IDB Project (2020): Index Database (https://www.indexdatabase.de/)
}
\author{
Andreas Schönberg
}
