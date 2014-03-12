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
If you want to do color graphing of the tracks within R I have provided a Mapper function to do this but it requires
gridded bathymetric data to create the background map.  The source for this example data set comes from the General
Bathymetric Chart of the Oceans or GEBCO at http://www.gebco.net.

You will need to get their GridViewer software and data, currently there is a link to get it at \cr
http://www.gebco.net/data_and_products/grid_display_software/

which at the time of writing sent me to \cr 
https://www.bodc.ac.uk/products/software_products/gebco_grid_display/

There are currently two options for the world data file, a 1 arc min
or 30 arc sec file.  The bathymetry data set is from the GEBCO_08 Grid a global 30 arc-second grid which is the larger
and more detailed data.  If you have a very wide ranging animal (albatros, elephant seal etc.), the smaller data set 
will probably be easier for R to handle and adequate for viewing the entire track, but you might want the more detailed
dataset to map portions of the track. Some experimentation by you may be necessary here.

This is fairly easy to use, just open the grid viewer and choose whatever way to select the portion of the globe you 
want to see, I used the "Exact Area" section in the Area tab.  This then produces a map of the data you selected and
you can then save the data that made that map in an ascii (*.asc) file.  

}

\examples{
data(bathymetry)
str(bathymetry)
bathymetryBogs<-subset(bathymetry,Long<=(-166) & Long >=(-169)
	& Lat<= 54.5 & Lat >=53,select=Long:Depth)

image.xyz=tapply(bathymetryBogs$Depth, list(bathymetryBogs$Long, bathymetryBogs$Lat), unique)
#create palatte for depth colors
Bathymetry.palatte<-colorRampPalette(brewer.pal(9, "Blues"),bias=3)
#Plot the background map image
image.plot(image.xyz,
	col=c(rev(Bathymetry.palatte(200)),terrain.colors(100)),#gray(0:20/20),
	breaks=round(c(seq(from=min(image.xyz),to=0,length.out=201),seq(from=max(image.xyz)/101
	,to=max(image.xyz),length.out=100)))
	#,smallplot=2 #plots legend off x axis
	)
\dontrun{
#If you want to map the entire bathymetry file, it takes a while
image.xyz=tapply(bathymetry$Depth, list(bathymetry$Long, bathymetry$Lat), unique)
#create palatte for depth colors
Bathymetry.palatte<-colorRampPalette(brewer.pal(9, "Blues"),bias=3)
#Plot the background map image
image.plot(image.xyz,
	col=c(rev(Bathymetry.palatte(200)),terrain.colors(100)),#gray(0:20/20),
	breaks=round(c(seq(from=min(image.xyz),to=0,length.out=201),seq(from=max(image.xyz)/101
	,to=max(image.xyz),length.out=100)))
	#,smallplot=2 #plots legend off x axis
	)
}
}
\keyword{datasets}
