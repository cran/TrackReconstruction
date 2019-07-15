\name{rawdata}
\alias{rawdata}
\docType{data}
\title{
Raw triaxial magnetomater and accelerometer data
}
\description{
Raw data file from a northern fur seal with date, time, internal temp, depth, triaxial magnetometer, accelerometer, and wet/dry fields.  
This data set is bookended by the first 7 gps points in the \code{gpsdata02} data set and is used in the examples for the \code{DeadReckoning},
\code{GeoReference}, \code{GraphLimits}, \code{Splitter} and \code{GeoRef} functions.
}
\usage{data(rawdata)}
\format{
  A data frame with 133100 observations on the following 9 variables.
  \describe{
    \item{\code{Date}}{a character vector}
    \item{\code{Time}}{a character vector}
    \item{\code{Depth}}{a numeric vector}
    \item{\code{MagSurge}}{a numeric vector}
    \item{\code{MagSway}}{a numeric vector}
    \item{\code{MagHeave}}{a numeric vector}
    \item{\code{AccSurge}}{a numeric vector}
    \item{\code{AccSway}}{a numeric vector}
    \item{\code{AccHeave}}{a numeric vector}
  }
}

\examples{
data(rawdata)
str(rawdata)
head(rawdata);tail(rawdata)
plot(rawdata$AccHeave)
}
\keyword{datasets}
