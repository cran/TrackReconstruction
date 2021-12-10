\name{bathymetry}
\alias{bathymetry}
\docType{data}
\title{
Bathymetry data for the Eastern Bering Sea
}
\description{
Bathymetry data for the Eastern Bering Sea at a resolution of 30 arc seconds downloaded from http://www.gebco.net
}
\usage{data(bathymetry)}
\format{
  A data frame with 1814400 observations on the following 3 variables.
  \describe{
    \item{\code{Long}}{a numeric vector}
    \item{\code{Lat}}{a numeric vector}
    \item{\code{Depth}}{a numeric vector}
  }
}
\details{
If you want to do color graphing of the tracks within R I have provided a \code{Mapper} function to do this but it requires
gridded (raster) bathymetric data to create the background map.  The source for this example data set comes from the General
Bathymetric Chart of the Oceans or GEBCO at \url{https://www.gebco.net}.
}

\examples{
data(bathymetry)
str(bathymetry)
head(bathymetry);tail(bathymetry)
bathymetryBogs<-subset(bathymetry,Long<=(-166) & Long >=(-169)
	& Lat<= 54.5 & Lat >=53,select=Long:Depth)

image.xyz=tapply(bathymetryBogs$Depth, list(bathymetryBogs$Long, bathymetryBogs$Lat), unique)
#create palette for depth colors
Bathymetry.palette<-colorRampPalette(brewer.pal(9, "Blues"),bias=3)
#Plot the background map image
image.plot(image.xyz,
	col=c(rev(Bathymetry.palette(200)),terrain.colors(100)),#gray(0:20/20),
	breaks=round(c(seq(from=min(image.xyz),to=0,length.out=201),seq(from=max(image.xyz)/101
	,to=max(image.xyz),length.out=100)))
	#,smallplot=2 #plots legend off x axis
	)
\dontrun{
#If you want to map the entire bathymetry file, it takes a while
image.xyz=tapply(bathymetry$Depth, list(bathymetry$Long, bathymetry$Lat), unique)
#create palette for depth colors
Bathymetry.palette<-colorRampPalette(brewer.pal(9, "Blues"),bias=3)
#Plot the background map image
image.plot(image.xyz,
	col=c(rev(Bathymetry.palette(200)),terrain.colors(100)),#gray(0:20/20),
	breaks=round(c(seq(from=min(image.xyz),to=0,length.out=201),seq(from=max(image.xyz)/101
	,to=max(image.xyz),length.out=100)))
	#,smallplot=2 #plots legend off x axis
	)
}
}
\keyword{datasets}
