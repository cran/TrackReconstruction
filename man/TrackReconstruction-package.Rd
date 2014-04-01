\name{TrackReconstruction-package}
\alias{TrackReconstruction-package}
\alias{TrackReconstruction}
\docType{package}
\title{

Reconstruct animal tracks from biologger data.  
}
\description{

Given accelerometer, magnetometer, depth and optional speed data, this package will reconstruct animal paths.  Given GPS data, 
pseudotracks can be geolocated.  The package was designed using northern fur seals (\code{Callorhinus ursinus}) but is probably suitable as a launching 
pad for other air breathing marine vertebtrates.
}
\details{
\tabular{ll}{
Package: \tab TrackReconstruction\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2014-04-01\cr
License: \tab GPL (>=2)\cr
}
Includes functions to Standardize logger data, format GPS data, find gaps in the raw data, navigation functions to 
caluclate distance, bearing, latitude and longitude, functions to calculate pseudotracks, to georeference the tracks
and finally to plot the tracks with color bathymetry.  A vignette includes some additional Perl code to deal with large data files as well
as additional plotting code to make 3D plots and 2D animations.

DeadReckoning

GapFinder

GeoReference

GeoRef (a wrapper for GeoReference)

GPStable

GraphLimits

Mapper

Navigation Functions (CalcBearing CalcLatitude CalcLongitude CalcDistance)

Splitter

Standardize
}
\author{
Brian Battaile

Maintainer:Brian Battaile <brian.battaile@gmail.com>
}
\references{
Wilson R.P., Liebsch,N., Davies,I.M., Quintana,F., Weimerskirch,H., Storch,S., Lucke,K., Siebert,U., Zankl,S., 
Muller,G., Zimmer,I., Scolaro,A., Campagna,C., Plotz,J., Bornemann,H., Teilmann,J. and Mcmahon,C.R. (2007) 
All at sea with animal tracks; methodological and analytical solutions for the resolution of movement.  
Deep-Sea Research II 54:193-210

Shepard E.L.C., Wilson, R.P., Halsey, L.G., Quintana, F., Laich, A.G., Gleiss, A.C., Liebsch, N., Myers, A.E., 
Norman, B.  (2008)  Derivation of body motion via appropriate smoothing of acceleration data.  
Aquatic Biology 4:235-241

Wilson R.P., Wilson M.P. (1988)  Dead reckoning: a new technique for determining penguim movements at sea.
Meeresforschung 32:2 155-158
}
\keyword{ package }
\seealso{
R packages diveMove, adehabitat, animalTrack, argosfilter and crawl
}
\examples{
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

\dontrun{
#plot the data with a bathymetric background, note how the axis dimensions have changed to 
#give a more realistic path relative to that produced in the simple plot call.  See the vignette
#or ?bathymetry for information on how to get bathymetric data for your study area.
data(bathymetry)
#This may take a minute or two
image.xyz=tapply(bathymetry$Depth, list(bathymetry$Long, bathymetry$Lat), unique)
Mapper(Georeferenced, gpsformat[2:7,], image.xyz, ExpFact = 500, minlat = 51, maxlat = 60,
minlong = -177, maxlong = -163, Title = "Fun Graph!!!")
#That entire animals trip plotted
data(georef1min02)
data(gpsdata02)
Mapper(georef1min02, gpsdata02, image.xyz, ExpFact = 500, minlat = 51, maxlat = 60,
minlong = -177, maxlong = -163, Title = "Fun Graph!!!")
}
}
