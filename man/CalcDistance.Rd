\name{CalcDistance}
\alias{CalcDistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Distance Between Two Points
}
\description{
Calculates the distance between two points on the globe given Latitude and Longitude in decimal degrees and returning
distance in kilometers.
}
\usage{
CalcDistance(initialLat, initialLong, finalLat, finalLong)
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
Latitude values south of the equator are 0 to -90.  This function is primarily used internally in the GPStable and 
GeoReference functions.
}
\value{
Distance in Kilometers
}
\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}
}
\author{
Brian Battaile
}

\examples{
##Distance from New York to Los Angeles
Bering<-CalcDistance(40.7697,-73.9735,34.0522,-118.2428)
Bering

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
