\name{GPStable}
\alias{GPStable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format GPS data
}
\description{
Takes DateTime, Latitude and Longitude in Decimal Degrees and converts Latitude and Longitude into radians and calculates bering and distance between consecutive locations 
}
\usage{
GPStable(rawdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawdata}{
data frame with DateTime, or Date and Time seperately, in same format as deadreckoning data, Latitude and Longitude 
in decimal degrees.  Must use those column labels.
}
}
\details{
Data must be provided in decimal degrees (e.g. 162.546).  Longitude values west of the prime meridian are 0 to -180.
Latitude values south of the equator are 0 to -90.  DateTime must be in the same format as output of DeadReckoning 
function or seperated as Date and Time that can be pasted together to create the same format as output of Deadreckoning 
function.  Distance is calculated using the Spherical Law of Cosines (see references).
}
\value{
Returns a data frame with columns DateTime, Latitude(Decimal Degrees), Longitude(Decimal Degrees), LatRad(Radians), LongRad(Radians), BeringRad, BeringDeg, DistanceKm.  BeringRad and BeringDeg are the Bering calculated from that point to the next point.  DistanceKm is the distance between that point and the previous point.
}
\references{
\url{http://www.movable-type.co.uk/scripts/latlong.html}
}
\author{
Brian Battaile
}

\examples{
data(gpsdata02)
head(gpsdata02)
gpsformat<-GPStable(gpsdata02)
head(gpsformat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line