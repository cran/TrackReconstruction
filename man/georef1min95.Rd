\name{georef1min95}
\alias{georef1min95}
\docType{data}
\title{
GeoReferenced fur seal track
}
\description{
Georeferenced data from a fur seal track in the Bering Sea.  Data has been thinned to relocations
every 1 minute.
}
\usage{data(georef1min95)}
\format{
  A data frame with 6566 observations on the following 6 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{LatRad}}{a numeric vector Latitude given in Radians}
    \item{\code{LongRad}}{a numeric vector Longitude given in Radians}
    \item{\code{Latitude}}{a numeric vector Latitude given in decimal degrees}
    \item{\code{Longitude}}{a numeric vector Longitude given in decimal degrees}
    \item{\code{Depth}}{a numeric vector in meters}
  }
}
\details{
Date files georef1min01, georef1min02, georef1min03 and georef1min26 have the same structure
}
\examples{
data(georef1min95)
str(georef1min95)
plot(georef1min95$Longitude, georef1min95$Latitude,pch=".")
data(gpsdata95)
points(gpsdata95$Longitude,gpsdata95$Latitude,col="red",pch="*")

}
\keyword{datasets}
