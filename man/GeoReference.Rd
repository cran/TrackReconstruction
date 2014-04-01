\name{GeoReference}
\alias{GeoReference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Georeference the Dead Reckoning data between two GPS points
}
\description{
Takes relocation data and forces it to go through two known points (such as GPS fixes) at the beginning and end by 
rotating the track and either expanding or contracting each section by a constant percentage.  The primary function used
in the wrapper function \code{GeoRef}.
}
\usage{
GeoReference(drdata, gpsdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{drdata}{
Data frame produced by the DeadReckoning function
}
  \item{gpsdata}{
Data frame produced by the \code{GPStable} function.
}
}
\details{
TimeDate stamp of first entry of drdata and gpsdata must be the same.  The \code{GeoRef} function does this 
for you my matching the DateTime stamp in the drdata and gpsdata.  The DateTime stamp of the drdata and gpsdata
must be in the same format.
}
\value{
Returns a data frame with DateTime, Distance, LatRad, LongRad, Latitude, Longitude, Depth, Speed, NewX, NewY, and Bearing
of the GeoReferenced DeadReckoning tracks.  NewX and NewY are cartesian coordinates in meters.  Distance is the distance 
from the origin to the new point in 2D.
}
\references{
Wilson R.P., Liebsch,N., Davies,I.M., Quintana,F., Weimerskirch,H., Storch,S., Lucke,K., Siebert,U., Zankl,S., 
Muller,G., Zimmer,I., Scolaro,A., Campagna,C., Plotz,J., Bornemann,H., Teilmann,J. and Mcmahon,C.R. (2007) 
All at sea with animal tracks; methodological and analytical solutions for the resolution of movement.  
Deep-Sea Research II 54:193-210

}
\author{
Brian Battaile
}

\examples{
#Standardize tag output
betas<-Standardize(1,1,-1,1,1,1,-57.8,68.76,-61.8,64.2,-70.16,58.08,
	-10.1,9.55,-9.75,9.72,-9.91,9.43)
#get declination and inclination data for study area
decinc<-c(10.228,65.918)
#data set with 7 associated GPS fixes in the "gpsdata02" data set
data(rawdata)
DRoutput<-DeadReckoning(rawdata,betas,decinc,Hz=16,RmL=2,DepthHz=1,SpdCalc=3,MaxSpd=3.5)
#prepare GPS data
data(gpsdata02)
gpsformat<-GPStable(gpsdata02)
Georeferenced<-GeoReference(DRoutput,gpsformat[c(2,3),])
plot(Georeferenced$Longitude,Georeferenced$Latitude,pch=".")
points(gpsformat$Longitude[2],gpsformat$Latitude[2],pch="S",col="Red")
points(gpsformat$Longitude[3],gpsformat$Latitude[3],pch="F",col="Blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
