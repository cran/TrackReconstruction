\name{CalcBearing}
\alias{CalcBearing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Bearing
}
\description{
Calculates Bearing given initial latitude and longitude and ending latitude and longitude in decimal degrees and returns
bearing in radians.
}
\usage{
CalcBearing(initialLat, initialLong, finalLat, finalLong)
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
Data must be provided in decimal degrees (e.g. 162.546).  Longitude values west of the prime meridian are 0 to -180.
Latitude values south of the equator are 0 to -90.  As one travels from point A to point B, the bearing to point B 
changes.  This function calculates the bearing from point A to point B at point A.  This function is primarily used 
internally in the \code{GPStable} and \code{GeoReference} functions.
}
\value{
Bearing in Radians
}
\references{
https://www.movable-type.co.uk/scripts/latlong.html
}
\author{
Brian Battaile
}
\examples{
#Bearing to Los Angeles from New York
Bearing<-CalcBearing(40.7697,-73.9735,34.0522,-118.2428)
Bearing
Bearing*360/(2*pi) #transform radians to degrees
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
