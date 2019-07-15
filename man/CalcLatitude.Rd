\name{CalcLatitude}
\alias{CalcLatitude}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Latitude
}
\description{
Calculates an unknown latitude given an initial latitude, distance traveled in meters and bearing.
}
\usage{
CalcLatitude(initialLat, distance, bearing)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialLat}{
initial Latitude
}
  \item{distance}{
distance in meters
}
  \item{bearing}{
bearing in radians
}
}
\details{
Data must be provided in decimal degrees (e.g.162.546).  Longitude values west of the prime meridian are 0 to -180.
Latitude values south of the equator are 0 to -90.  Calculates an unknown latutude given an initial latitude, distance traveled in meters and bearing
in radians.  This function is primarily used internally in the \code{gpstable} and \code{GeoReference} functions.
}
\value{
Latitude in radians
}
\references{
https://www.movable-type.co.uk/scripts/latlong.html
}
\author{
Brian Battaile
}
\examples{
#Calculate the latitude of Los Angeles given a starting point in Manhattan and the distance 
#and bearing to Los Angeles.
Lat<-CalcLatitude (40.7697,3938000,-1.507516)
#transform to degrees
Lat*360/(2*pi)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
