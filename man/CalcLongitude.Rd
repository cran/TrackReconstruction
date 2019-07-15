\name{CalcLongitude}
\alias{CalcLongitude}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Longitude
}
\description{
Calculate the longitude of a destination given initial latitude and longitude, distance traveled, bearing and final latitude.
}
\usage{
CalcLongitude(initialLat, destinationLat, initialLong, distance, bearing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialLat}{
initial Latitude
}
  \item{destinationLat}{
final Latitude
}
  \item{initialLong}{
initial Longitude
}
  \item{distance}{
distance from initial to final in meters
}
  \item{bearing}{
bearing from point of departure to final destination
}
}
\details{
Calculates a destination's longitude given initial latitude and longitude in degrees (e.g.162.546, Longitude values west 
of the prime meridian are 0 to -180.  Latitude values south of the equator are 0 to -90), destination latitude in 
RADIANS (result of \code{CalcLatitude} function), distance traveled in meters and bearing in radians.  This function is 
primarily used internally in the \code{GPStable} and \code{GeoReference} functions.
}
\value{
Longitude of final destination in radians.
}
\references{
https://www.movable-type.co.uk/scripts/latlong.html
}
\author{
Brian Battaile
}

\examples{
#Longitude of Los Angeles starting in Manhattan
Long <- CalcLongitude(40.7697,0.5943228,-73.9735,3938000,-1.507516)
#transform to degrees
Long*360/(2*pi)

##Use results of CalcLatitude
Lat<-CalcLatitude (40.7697,3938000,-1.507516)
Long <- CalcLongitude(40.7697,Lat,-73.9735,3938000,-1.507516)
#transform to degrees
Long*360/(2*pi)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
