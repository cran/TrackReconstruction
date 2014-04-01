\name{rawdatagap}
\alias{rawdatagap}
\docType{data}
\title{
Raw biologger data with a gap
}
\description{
Raw data file from a fur seal with a gap in the data file.  Used as an example in the \code{GapFinder} function.
}
\usage{data(rawdatagap)}
\format{
  A data frame with 13738 observations on the following 9 variables.
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
data(rawdatagap)
head(rawdatagap);tail(rawdatagap)
str(rawdatagap)
plot(rawdatagap$AccHeave)
}
\keyword{datasets}
