\name{gpsdata26}
\alias{gpsdata26}
\docType{data}
\title{
GPS raw data
}
\description{
Complete gps file for northern fur seal track in the Bering Sea.  Data taken by a wildlife computers mk10-F tag.  
This data set is used as an example in the \code{Mapper} function.  \code{georef1min26} is the complementary pseudotrack data for these gps points.
}
\usage{data(gpsdata26)}
\format{
  A data frame with 9 observations on the following 3 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
  }
}

\examples{
data(gpsdata26)
head(gpsdata26);tail(gpsdata26)
str(gpsdata26)
plot(gpsdata26$Longitude,gpsdata26$Latitude)
}
\keyword{datasets}
