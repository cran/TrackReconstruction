\name{georef1min01}
\alias{georef1min01}
\docType{data}
\title{
GeoReferenced fur seal track
}
\description{
Georeferenced data from a fur seal track in the Bering Sea.  Data has been thinned to relocations
every 1 minute.  This data set is used as an example in the vignette.  gpsdata01 is the complementary gps data for this pseudotrack.
}
\usage{data(georef1min01)}
\format{
  A data frame with 6681 observations on the following 6 variables.
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
Date files georef1min02, georef1min03, georef1min26 and georef1min95 have the same structure
}
\examples{
data(georef1min01)
head(georef1min01);tail(georef1min01)
str(georef1min01)
plot(georef1min01$Longitude, georef1min01$Latitude,pch=".")
data(gpsdata01)
points(gpsdata01$Longitude,gpsdata01$Latitude,col="red",pch="*")

}
\keyword{datasets}
