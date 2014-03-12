\name{gpsdata03}
\alias{gpsdata03}
\docType{data}
\title{
GPS raw data
}
\description{
Complete raw gps file for northern fur seal track in the Bering Sea.  Data taken by a wildlife computers mk10-F tag.
}
\usage{data(gpsdata03)}
\format{
  A data frame with 57 observations on the following 3 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
  }
}

\examples{
data(gpsdata03)
str(gpsdata03)
plot(gpsdata03$Longitude,gpsdata03$Latitude)
}
\keyword{datasets}
