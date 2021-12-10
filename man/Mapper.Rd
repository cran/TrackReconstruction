\name{Mapper}
\alias{Mapper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Animal track plotting with a color map
}
\description{
Makes a color map of your animal track with terrain colors for land and shades of blue for depth
}
\usage{
Mapper(inFile, gpsFile, bathyFile, ExpFact = 500, minlat = 51, maxlat = 60,
minlong = -177, maxlong = -163, Title = "Main")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inFile}{
Track file of latitude and longitude colums with those names (output of \code{GeoReference} function)
}
  \item{gpsFile}{
GPS file of latitude and longitude colums with those names
}
  \item{bathyFile}{
Basemap file made from latitude, longitude and depth data, see \code{?bathymetry} for format details
}
  \item{ExpFact}{
Expansion Factor- a multiplier of how smooth you want your new background graph to be.  Larger numbers make it look
smoother.  Experiment with it.
}
  \item{minlat}{
minimum latitude of the data from the image.xyz file, MUST BE A WHOLE NUMBER
}
  \item{maxlat}{
maximum latitude of the data from the image.xyz file, MUST BE A WHOLE NUMBER
}
  \item{minlong}{
minimum longitude of the data from the image.xyz file, MUST BE A WHOLE NUMBER
}
  \item{maxlong}{
maximum longitude of the data from the image.xyz file, MUST BE A WHOLE NUMBER
}
  \item{Title}{
Title of your graph
}
}
\details{
See \url{https://www.gebco.net} do download the raw bathymetric data to create the \code{bathyFile}.
}

\value{
Given a bathymetric data file that is much larger than a given track, this function zooms the graph into where the data are
and smooths out the background data if they are pixilated.
The idea is to have a single bathymetry file to make many graphs for many animals no matter where they go so long as the tracks are all
within the area of the bathymetry file limits.  It creates a square graph that should adjust for the differences in actual
distances between latitude and longitude graduals as latitude changes.
}

\author{
Brian Battaile
}

\examples{
#A file from GeoRef function that has been thinned to data every 1 minute
data(georef1min26)
data(gpsdata26)
#See the vignette or ?bathymetry for information on how to get 
#bathymetric data for your study area
data(bathymetry)
#This subset is just to save time creating the image.xyz
bathymetryBogs<-subset(bathymetry,Long<=(-168)& Long >=(-169)
	& Lat<= 55 & Lat >=53,select=Long:Depth)
image.xyzBogs=tapply(bathymetryBogs$Depth, list(bathymetryBogs$Long, bathymetryBogs$Lat), unique)
Mapper(georef1min26, gpsdata26, image.xyzBogs, ExpFact = 500, minlat = 53, 
	maxlat = 55, minlong = -169, maxlong = -168, Title = "Fun Graph!!!")

\dontrun{
#Plot the entire bathymetry file (this will take a minute or two)
image.xyz=tapply(bathymetry$Depth, list(bathymetry$Long, bathymetry$Lat), unique)
Bathymetry.palatte<-colorRampPalette(brewer.pal(9, "Blues"),bias=3)
#Plot the background map image
image.plot(image.xyz,
	col=c(rev(Bathymetry.palatte(200)),terrain.colors(100)),#gray(0:20/20),
	breaks=round(c(seq(from=min(image.xyz),to=0,length.out=201),seq(from=max(image.xyz)/101
	,to=max(image.xyz),length.out=100)))
	#,smallplot=2 #plots legend off x axis
	)
#The next examples plot only the part of the background map that contains the relocation data
#Example 1
data(georef1min02)
data(gpsdata02)
Mapper(georef1min02, gpsdata02, image.xyz, ExpFact = 500, minlat = 51, maxlat = 60,
minlong = -177, maxlong = -163, Title = "Fun Graph!!!")

#Example 2
bathymetryBogs2<-subset(bathymetry,Long<=(-168)& Long >=(-169.2)
	& Lat<= 54.3 & Lat >=53.1,select=Long:Depth)
image.xyzBogs2=tapply(bathymetryBogs2$Depth, list(bathymetryBogs2$Long, bathymetryBogs2$Lat),
	unique)
image.plot(image.xyzBogs2,
	col=c(rev(Bathymetry.palatte(200)),terrain.colors(100)),#gray(0:20/20),
	breaks=round(c(seq(from=min(image.xyz),to=0,length.out=201),seq(from=max(image.xyz)/101
	,to=max(image.xyz),length.out=100)))
	#,smallplot=2 #plots legend off x axis
	)
win.graph()
#Note the reduction in pixelation
data(georef1min95)
data(gpsdata95)
Mapper(georef1min95, gpsdata95, image.xyz, ExpFact = 500, minlat = 51, maxlat = 60,
minlong = -177, maxlong = -163, Title = "Fun Graph!!!")
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

