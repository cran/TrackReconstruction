\name{CalcBering}
\alias{CalcBering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Bering
}
\description{
Calculates Bering given initial Latitude and Longitude and ending Latitude and Longitude in decimal degrees and returns
Bering in radians.
}
\usage{
CalcBering(initialLat, initialLong, finalLat, finalLong)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialLat}{
initial Latitude
}
  \item{initialLong}{
initial Longitude
}
  \item{finalLat}{
ending Latitude
}
  \item{finalLong}{
ending Longitude
}
}
\details{
Data must be provided in decimal degrees (e.g.162.546).  Longitude values west of the prime meridian are 0 to -180.
Latitude values south of the equator are 0 to -90.  As one travels from point A to point B, the Bering to point B 
changes.  This function calculates the Bering from point A to point B AT point A.  This function is primarily used 
internally in the GPStable and GeoReference functions.
}
\value{
Bering in Radians
}
\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}
}
\author{
Brian Battaile
}
\examples{
#Bering to Los Angeles from New York
Bering<-CalcBering(40.7697,-73.9735,34.0522,-118.2428)
Bering
Bering*360/(2*pi) #transform radians to degrees
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
