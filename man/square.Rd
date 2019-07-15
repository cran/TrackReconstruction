\name{square}
\alias{square}
\docType{data}
\title{
Raw triaxial magnetomater and accelerometer data
}
\description{
Fabricated data file 100 observations long to make a square.  Useful for exercises to understand how the 
\code{DeadReckoning} algorithm works.
}
\usage{data(rawdata)}
\format{
  A data frame with 100 observations on the following 10 variables.
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
    \item{\code{Speed}}{a numeric vector}
  }
}

\examples{
data(square)
str(square)
head(square);tail(square)
}
\keyword{datasets}
