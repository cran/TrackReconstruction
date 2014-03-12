\name{CalcLatitude}
\alias{CalcLatitude}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Latitude
}
\description{
Calculates an unknown Latitude given an initial Latitude, distance traveled in meters and bering.
}
\usage{
CalcLatitude(initialLat, distance, bering)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialLat}{
initial Latitude
}
  \item{distance}{
distance in meters
}
  \item{bering}{
bering in radians
}
}
\details{
Data must be provided in decimal degrees (e.g.162.546).  Longitude values west of the prime meridian are 0 to -180.
Latitude values south of the equator are 0 to -90.  Calculates an unknown Latutude given an initial Latitude, distance traveled in meters and bering
in radians.  This function is primarily used internally in the gpstable and GeoReference functions.
}
\value{
Latitude in radians
}
\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}
}
\author{
Brian Battaile
}
\examples{
##Latitude of Los Angeles starting in Manhattan
Lat<-CalcLatitude (40.7697,3938000,-1.507516)
#transform to degrees
Lat*360/(2*pi)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
