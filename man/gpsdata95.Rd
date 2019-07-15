\name{gpsdata95}
\alias{gpsdata95}
\docType{data}
\title{
GPS raw data
}
\description{
Complete gps file for northern fur seal track in the Bering Sea.  Data taken by a wildlife computers mk10-F tag.  
This data set is used as an example in the \code{Mapper} function.  \code{georef1min95} is the complementary pseudotrack data for these gps points.
}
\usage{data(gpsdata95)}
\format{
  A data frame with 93 observations on the following 3 variables.
  \describe{
    \item{\code{DateTime}}{a character vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
  }
}

\examples{
data(gpsdata95)
head(gpsdata95);tail(gpsdata95)
str(gpsdata95)
plot(gpsdata95$Longitude,gpsdata95$Latitude)
}
\keyword{datasets}
