\name{gpsdata02}
\alias{gpsdata02}
\docType{data}
\title{
GPS raw data
}
\description{
Complete gps file for northern fur seal track in the Bering Sea.  Data taken by a wildlife computers mk10-F tag.  
This data set is used as an example in the \code{Mapper}, \code{GraphLimits}, \code{GeoRef}, \code{Splitter} and \code{GeoReference}
functions and in the vignette.  georef1min02 is the complementary pseudotrack data for these gps points.
}
\usage{data(gpsdata02)}
\format{
  A data frame with 276 observations on the following 3 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
  }
}

\examples{
data(gpsdata02)
head(gpsdata02);tail(gpsdata02)
str(gpsdata02)
plot(gpsdata02$Longitude,gpsdata02$Latitude)
}
\keyword{datasets}
