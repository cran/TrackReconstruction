\name{CalcLongitude}
\alias{CalcLongitude}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Longitude
}
\description{
Calculate the Longitude of a destination given initial Latitude and Longitude, distance traveled, bering and final Latitude.
}
\usage{
CalcLongitude(initialLat, matchingLat, initialLong, distance, bering)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialLat}{
initial Latitude
}
  \item{matchingLat}{
final Latitude
}
  \item{initialLong}{
initial Longitude
}
  \item{distance}{
distance from initial to final in meters
}
  \item{bering}{
bering from point of departure to final destination
}
}
\details{
Calculates a destinations Longitude given initial Latitude and Longitude in degrees (e.g.162.546, Longitude values west 
of the prime meridian are 0 to -180.  Latitude values south of the equator are 0 to -90), destination Latitude in 
RADIANS (result of CalcLatitude function), distance traveled in meters and bering in radians.  This function is 
primarily used internally in the GPStable and GeoReference functions.
}
\value{
Longitude of final destination in radians.
}
\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}
}
\author{
Brian Battaile
}

\examples{
##Latitude of Los Angeles starting in Manhattan
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
