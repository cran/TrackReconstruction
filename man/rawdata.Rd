\name{rawdata}
\alias{rawdata}
\docType{data}
\title{
Raw triaxial magnetomater and accelerometer data
}
\description{
Raw data file from a northern fur seal with date, time, internal temp, depth, triaxial magnetometer, accelerometer, and wet/dry fields.
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
## maybe str(rawdata) ; plot(rawdata) ...
}
\keyword{datasets}
