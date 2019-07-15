\name{GeoRef}
\alias{GeoRef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Georeference the Dead Reckoning data
}
\description{
A wrapper function for the \code{GeoReference} function that georeferences a deadreckoning track with multiple GPS relocations.
}
\usage{
GeoRef(drdata, gpsfdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{drdata}{
Data frame from, or similar in format as that produced by the \code{DeadReckoning} function. 
}
  \item{gpsfdata}{
Data frame from, or similar in format as that produced by the \code{GPStable} function.
}
}
\details{
Data that is not bookended by the DateTime stamp of the GPS relocations will be discarded.  The DateTime stamp of the drdata and gpsfdata
must be in the same format, see the \code{strptime} function help file for details.
}
\value{
Returns a data frame with DateTime, Distance, LatRad, LongRad, Latitude, Longitude, Depth, Speed, NewX, NewY, and Bering
of the Georeferenced Deadreckoned tracks.  NewX and NewY are cartesian coordinates in meters.  Distance is the distance 
from the origin to the georeferenced point in 2D.
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
	-10.1,9.55,-9.75,9.72, -9.91,9.43)
#get declination and inclination data for study area
decinc<-c(10.228,65.918)
#data set with 6 associated GPS fixes in the "gpsdata" data set
data(rawdata)
DRoutput<-DeadReckoning(rawdata,betas,decinc,Hz=16,RmL=2,DepthHz=1,SpdCalc=3,MaxSpd=3.5)
#prepare GPS data
data(gpsdata02)
gpsformat<-GPStable(gpsdata02)
Georeferenced<-GeoRef(DRoutput,gpsformat)
plot(Georeferenced$Longitude,Georeferenced$Latitude,pch=".")
points(gpsformat$Longitude[2],gpsformat$Latitude[2],pch="S",col="Red") #Start
points(gpsformat$Longitude[7],gpsformat$Latitude[7],pch="F",col="Blue") #Finish
#Intermediate GPS points
points(gpsformat$Longitude[3:6],gpsformat$Latitude[3:6],pch="*",col="Red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
