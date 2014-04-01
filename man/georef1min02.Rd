\name{georef1min02}
\alias{georef1min02}
\docType{data}
\title{
GeoReferenced fur seal track
}
\description{
Georeferenced data from a fur seal track in the Bering Sea.  Data has been thinned to relocations
every 1 minute.  This data set is used as an example in the vignette and the \code{Mapper} function.  
gpsdata02 is the complementary gps data for this pseudotrack.
}
\usage{data(georef1min02)}
\format{
  A data frame with 9147 observations on the following 6 variables.
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
Date files georef1min01, georef1min03, georef1min26 and georef1min95 have the same structure
}
\examples{
data(georef1min02)
head(georef1min02);tail(georef1min02)
str(georef1min02)
plot(georef1min02$Longitude, georef1min02$Latitude,pch=".")
data(gpsdata02)
points(gpsdata02$Longitude,gpsdata02$Latitude,col="red",pch="*")

}
\keyword{datasets}
