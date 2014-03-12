\name{gpsdata01}
\alias{gpsdata01}
\docType{data}
\title{
GPS raw data
}
\description{
Complete raw gps file for northern fur seal track in the Bering Sea.  Data taken by a wildlife computers mk10-F tag.
}
\usage{data(gpsdata01)}
\format{
  A data frame with 233 observations on the following 3 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
  }
}

\examples{
data(gpsdata01)
str(gpsdata01)
plot(gpsdata01$Longitude,gpsdata01$Latitude)
}
\keyword{datasets}
