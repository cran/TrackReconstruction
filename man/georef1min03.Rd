\name{georef1min03}
\alias{georef1min03}
\docType{data}
\title{
GeoReferenced fur seal track
}
\description{
Georeferenced data from a fur seal track in the Bering Sea.  Data has been thinned to relocations
every 1 minute.  This data set is used as an example in the vignette.  \code{gpsdata03} is the complementary gps data for this pseudotrack.
}
\usage{data(georef1min03)}
\format{
  A data frame with 6724 observations on the following 6 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{LatRad}}{a numeric vector, latitude given in radians}
    \item{\code{LongRad}}{a numeric vector, longitude given in radians}
    \item{\code{Latitude}}{a numeric vector, latitude given in decimal degrees}
    \item{\code{Longitude}}{a numeric vector, longitude given in decimal degrees}
    \item{\code{Depth}}{a numeric vector, in meters}
  }
}
\details{
Date files \code{georef1min01}, \code{georef1min02}, \code{georef1min26} and \code{georef1min95} have the same structure
}
\examples{
data(georef1min03)
head(georef1min03);tail(georef1min03)
str(georef1min03)
plot(georef1min03$Longitude, georef1min03$Latitude,pch=".")
data(gpsdata03)
points(gpsdata03$Longitude,gpsdata03$Latitude,col="red",pch="*")
}
\keyword{datasets}
