\name{georef1min95}
\alias{georef1min95}
\docType{data}
\title{
GeoReferenced fur seal track
}
\description{
Georeferenced data from a fur seal track in the Bering Sea.  Data has been thinned to relocations
every 1 minute.  This data set is used as an example in the \code{Mapper} function.  
\code{gpsdata95} is the complementary gps data for this pseudotrack.
}
\usage{data(georef1min95)}
\format{
  A data frame with 6566 observations on the following 6 variables.
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
Date files \code{georef1min01}, \code{georef1min02}, \code{georef1min03} and \code{georef1min26} have the same structure
}
\examples{
data(georef1min95)
head(georef1min95);tail(georef1min95)
str(georef1min95)
plot(georef1min95$Longitude, georef1min95$Latitude,pch=".")
data(gpsdata95)
points(gpsdata95$Longitude,gpsdata95$Latitude,col="red",pch="*")

}
\keyword{datasets}
