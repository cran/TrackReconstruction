\name{GraphLimits}
\alias{GraphLimits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine Latitude and Longitude limits of a graph
}
\description{
Finds the minimum and maximum of the longitude and latitude and then adds on 2.5-5\% of the Lat and Long to create 
boarders in the graph
}
\usage{
GraphLimits(infile)
}
\arguments{
  \item{infile}{
A file with columns of latitude and longitude labeled as Latitude and Longitude
}
}
\details{
Internal function used in the Mapper function.
}
\value{
Returns a list of 4 scalars defining the boarders of the data
}

\author{
Brian Battaile
}

\examples{
#Standardize tag output
betas<-Standardize(1,1,-1,1,1,1,-57.8,68.76,-61.8,64.2,-70.16,58.08,-10.1,9.55,-9.75,9.72,
-9.91,9.43)
#get declination and inclination data for study area
decinc<-c(10.228,65.918)
#data set with 11 associated GPS fixes in the "gpsdata" data set
data(rawdata)
DRoutput<-DeadReckoning(rawdata ,betas, decinc, Hz = 16, RmL = 2, DepthHz = 1, SpdCalc=3,
MaxSpd=3.5)
#prepare GPS data
data(gpsdata02)
gpsformat<-GPStable(gpsdata02)
Georeferenced<-GeoReference(DRoutput,gpsformat[1:2,])
Limits<-GraphLimits(Georeferenced)
Limits
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
